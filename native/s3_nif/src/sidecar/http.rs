use anyhow::Error;
use axum::{
    Json, Router,
    body::Body,
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode,HeaderValue},
    response::Response,
    routing::{get, post},
};
use futures::TryStreamExt;
use serde::Serialize;
use serde_json::json;
use tokio_util::io::ReaderStream;
use tower_http::cors::CorsLayer;
use x402_rs::network::{USDCDeployment, Network};
use x402_axum::{IntoPriceTag};
use x402_rs::types::{EvmAddress, Scheme, PaymentRequirements, Base64Bytes};
use std::str::FromStr;
use x402_axum::layer::X402Paygate;
use std::sync::Arc;
use x402_axum::facilitator_client::FacilitatorClient;

use crate::sidecar::jwt::create_signed_dataitem_url;

use crate::s3::{DATAITEMS_BUCKET, DATAITEMS_DIR, create_s3_client, get_object};
use crate::sidecar::{AppState, SIDECAR_SERVER_ENDPOINT, FACILITATOR_URL, ans104, get_env_var, range};

#[derive(serde::Deserialize)]
struct ResolveQuery {
    range: Option<String>,
    token: Option<String>,
}

#[derive(Debug)]
struct SidecarConfig {
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    port: String,
    jwk_priv: String,
    base_url: String
}

#[derive(Serialize)]
struct SignedUrlResponse {
    signed_url: String,
}

impl SidecarConfig {
    pub fn load_env() -> Result<Self, Error> {
        Ok(Self {
            endpoint: get_env_var("ENDPOINT")?,
            access_key_id: get_env_var("ACCESS_KEY_ID")?,
            secret_access_key: get_env_var("SECRET_ACCESS_KEY")?,
            region: get_env_var("REGION")?,
            port: get_env_var("PORT")?,
            jwk_priv: get_env_var("PRESIGNED_URL_JWT_PRIV")?,
            base_url: get_env_var("BASE_URL")?
        })
    }
}

fn get_header(headers: &HeaderMap, name: &str) -> Result<String, StatusCode> {
    headers
        .get(name)
        .and_then(|h| h.to_str().ok())
        .map(String::from)
        .ok_or(StatusCode::BAD_REQUEST)
}

fn validate_api_key(headers: &HeaderMap) -> Result<(), StatusCode> {
    let auth_header = get_header(headers, "authorization")?;
    let token = auth_header
        .strip_prefix("Bearer ")
        .ok_or(StatusCode::UNAUTHORIZED)?;
    if token != get_env_var("SECRET_ACCESS_KEY").unwrap_or_default() {
        return Err(StatusCode::UNAUTHORIZED);
    }
    Ok(())
}

pub async fn serve() -> Result<(), Box<dyn std::error::Error>> {
    let sidecar_config = SidecarConfig::load_env()?;

    println!(
        "offchain ANS-104 streaming sidecar v{}",
        env!("CARGO_PKG_VERSION")
    );
    println!("running on port {}", sidecar_config.port);

    let s3_client = create_s3_client(
        &sidecar_config.endpoint,
        &sidecar_config.access_key_id,
        &sidecar_config.secret_access_key,
        &sidecar_config.region,
        Some(true),
    )
    .await;

    let x402_facilitator = Arc::new(FacilitatorClient::try_from(FACILITATOR_URL)?);

    let app_state = AppState { 
        s3_client,
        x402_facilitator,
    };

    let app = Router::new()
        .route("/resolve/{*dataitem_key}", get(resolve_dataitem))
        .route("/health", get(|| async { "sidecar running" }))
        .route("/sign", post(create_signed_url))
        .route("/protected/resolve/{payee}/{*dataitem_key}", get(resolve_protected_dataitem))
        .layer(CorsLayer::permissive())
        .with_state(app_state);

    let listener =
        tokio::net::TcpListener::bind(format!("{}:{}",sidecar_config.base_url, sidecar_config.port)).await?;

    axum::serve(listener, app).await?;
    Ok(())
}

async fn resolve_dataitem(
    Path(dataitem_key): Path<String>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    resolve_dataitem_impl(dataitem_key, params, headers, state).await
}

async fn resolve_protected_dataitem(
    Path((pay_to, dataitem_key)): Path<(String, String)>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    let sidecar_config = SidecarConfig::load_env().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    // 402 payment requirements for this specific request
    let payee = EvmAddress::from_str(&pay_to)
        .map_err(|_| StatusCode::BAD_REQUEST)?;
    let usdc_deployment = USDCDeployment::by_network(Network::PolygonAmoy)
        .pay_to(payee)
        .amount(0.01)
        .unwrap();
    
    let payment_requirements = vec![PaymentRequirements {
        scheme: Scheme::Exact,
        network: Network::PolygonAmoy,
        max_amount_required: usdc_deployment.amount,
        resource: format!("http://{}:{}/protected/{}/{}", sidecar_config.base_url, sidecar_config.port, pay_to, dataitem_key)
            .parse()
            .unwrap(),
        description: "premium dataitem access".to_string(),
        mime_type: "application/octet-stream".to_string(),
        pay_to: usdc_deployment.pay_to,
        max_timeout_seconds: 300,
        asset: usdc_deployment.token.address(),
        extra: Some(serde_json::json!({
            "name": "USDC",
            "version": "2"
        })),
        output_schema: None,
    }];
    
    let paygate = X402Paygate {
        facilitator: state.x402_facilitator.clone(),
        payment_requirements: Arc::new(payment_requirements),
    };
    
    // payment verification
    let payment_payload = paygate
        .extract_payment_payload(&headers)
        .await
        .map_err(|_| StatusCode::PAYMENT_REQUIRED)?;
    
    let verify_request = paygate
        .verify_payment(payment_payload)
        .await
        .map_err(|_| StatusCode::PAYMENT_REQUIRED)?;
    
    let response = resolve_dataitem_impl(dataitem_key, params, headers.clone(), state.clone()).await?;
    
    // settle payment after successful dataitem resolving
    let settlement = paygate
        .settle_payment(&verify_request)
        .await
        .map_err(|_| StatusCode::PAYMENT_REQUIRED)?;
    
    let payment_response_header: Base64Bytes = settlement
        .try_into()
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    let mut final_response = response;
    final_response.headers_mut().insert(
        "x-payment-response",
        HeaderValue::from_bytes(payment_response_header.as_ref())
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
    );
    
    Ok(final_response)
}

async fn resolve_dataitem_impl(
    dataitem_key: String,
    params: ResolveQuery,
    headers: HeaderMap,
    state: AppState,
) -> Result<Response, StatusCode> {
    println!("Resolving dataitem key: {dataitem_key}");

    let range_str = params
        .range
        .or_else(|| {
            headers
                .get("range")
                .and_then(|h| h.to_str().ok().map(String::from))
        })
        .unwrap_or_default();

    // determine bucket and key based on jwt token presence
    let (bucket_name, key) = if let Some(token) = params.token {
        // private dataitem
        let claims = match crate::sidecar::jwt::validate_dataitem_token(&token, &dataitem_key) {
            Ok(claims) => claims,
            Err(_) => {
                return Ok(Response::builder()
                    .status(StatusCode::UNAUTHORIZED)
                    .header("content-type", "application/json")
                    .body(
                        serde_json::to_string(
                            &json!({"error": "invalid token or it reached expiration timestamp"}),
                        )
                        .unwrap()
                        .into(),
                    )
                    .unwrap());
            }
        };

        let bucket = claims.bucket_name;
        (bucket, dataitem_key)
    } else {
        // public dataitem
        let bucket = DATAITEMS_BUCKET.to_string();
        let key = format!("{DATAITEMS_DIR}/{dataitem_key}.ans104");
        (bucket, key)
    };

    let header_obj = get_object(&state.s3_client, &bucket_name, &key, "bytes=0-2047")
        .await
        .map_err(|_| StatusCode::NOT_FOUND)?;
    let header_bytes = header_obj
        .body
        .collect()
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
        .into_bytes()
        .to_vec();

    let (mime_type, data_offset) = ans104::parse_ans104_header(&header_bytes)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    if range_str.is_empty() {
        stream_data_section(
            &state.s3_client,
            &key,
            &bucket_name,
            data_offset,
            None,
            &mime_type,
        )
        .await
    } else {
        let (start, end_opt) =
            range::parse_range(&range_str).ok_or(StatusCode::RANGE_NOT_SATISFIABLE)?;
        stream_data_section(
            &state.s3_client,
            &key,
            &bucket_name,
            data_offset,
            Some((start, end_opt)),
            &mime_type,
        )
        .await
    }
}

async fn stream_data_section(
    client: &aws_sdk_s3::Client,
    key: &str,
    bucket_name: &str,
    data_offset: usize,
    range: Option<(u64, Option<u64>)>,
    mime_type: &str,
) -> Result<Response, StatusCode> {
    let s3_range = match range {
        Some((start, end_opt)) => {
            let phys_start = data_offset as u64 + start;
            match end_opt {
                Some(end) => format!("bytes={phys_start}-{}", data_offset as u64 + end),
                None => format!("bytes={phys_start}-"),
            }
        }
        None => format!("bytes={data_offset}-"),
    };

    let obj = get_object(client, bucket_name, key, &s3_range)
        .await
        .map_err(|_| StatusCode::NOT_FOUND)?;

    let content_length = obj.content_length().unwrap_or(0) as u64;
    let reader = obj.body.into_async_read();
    let stream = ReaderStream::new(reader).map_err(std::io::Error::other);
    let body = Body::from_stream(stream);

    let mut response = Response::builder()
        .header("content-type", mime_type)
        .header("content-length", content_length.to_string())
        .header("accept-ranges", "bytes")
        .header("access-control-allow-origin", "*")
        .header("access-control-allow-headers", "*")
        .header("access-control-allow-methods", "GET, OPTIONS");

    if range.is_some() {
        response = response.status(StatusCode::PARTIAL_CONTENT);
        if let Some((start, end_opt)) = range {
            let end = end_opt.unwrap_or(start + content_length - 1);
            response = response.header(
                "content-range",
                format!("bytes {start}-{end}/{content_length}"),
            );
        }
    }

    response
        .body(body)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

async fn create_signed_url(headers: HeaderMap) -> Result<Json<SignedUrlResponse>, StatusCode> {
    println!("function called");
    validate_api_key(&headers)?;

    println!("passed validate_api_key");

    let bucket_name = get_header(&headers, "x-bucket-name")?;
    let load_acc = get_header(&headers, "x-load-acc")?;
    let dataitem_key = get_header(&headers, "x-dataitem-key")?;
    let expires_minutes = get_header(&headers, "x-expires-minutes")
        .unwrap_or("60".to_string())
        .parse::<i64>()
        .unwrap_or(60);

    let token = create_signed_dataitem_url(&bucket_name, &load_acc, &dataitem_key, expires_minutes)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let signed_url = format!("{SIDECAR_SERVER_ENDPOINT}/resolve/{dataitem_key}?token={token}");

    Ok(Json(SignedUrlResponse { signed_url }))
}
