use axum::{
    Json,
    body::Body,
    extract::{Path, Query, State},
    http::{HeaderMap, HeaderValue, StatusCode},
    response::Response,
};
use base64::{Engine as _, engine::general_purpose};
use futures::TryStreamExt;
use serde_json::{Value, json};
use std::str::FromStr;
use std::sync::Arc;
use tokio_util::io::ReaderStream;
use x402_axum::IntoPriceTag;
use x402_axum::layer::X402Paygate;
use x402_rs::types::{Base64Bytes, EvmAddress, PaymentRequirements, Scheme};

use crate::sidecar::{jwt::create_signed_dataitem_url, x402104::Network402104};

use crate::s3::{DATAITEMS_BUCKET, DATAITEMS_DIR, get_object};
use crate::sidecar::{
    AppState, FACILITATOR_URL, SIDECAR_SERVER_ENDPOINT, ans104, get_env_var, range,
};

use crate::sidecar::http::{
    Payee402, ResolveQuery, SidecarConfig, Signed402UrlResponse, SignedUrlResponse, get_header,
    validate_api_key,
};

pub async fn root() -> Json<Value> {
    Json(json!({
        "version": env!("CARGO_PKG_VERSION"),
        "running": true,
        "x402-enabled": true,
        "private-dataitems": true,
        "x402-facilitator": FACILITATOR_URL,
        "name": "s3-node-1"
    }))
}

pub async fn resolve_dataitem_normal(
    Path(dataitem_key): Path<String>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    resolve_dataitem_impl(dataitem_key, None, false, params, headers, state, false, false).await
}

pub async fn resolve_dataitem_fast(
    Path(dataitem_key): Path<String>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    resolve_dataitem_impl(dataitem_key, None, false, params, headers, state, true, false).await
}

pub async fn resolve_dataitem_preview(
    Path(dataitem_key): Path<String>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    resolve_dataitem_impl(dataitem_key, None, false, params, headers, state, false, true).await
}

pub async fn resolve_dataitem_preview_fast(
    Path(dataitem_key): Path<String>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    resolve_dataitem_impl(dataitem_key, None, false, params, headers, state, true, true).await
}

pub async fn download_dataitem_binary(
    Path(dataitem_key): Path<String>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    resolve_dataitem_impl(dataitem_key, None, true, params, headers, state, false, false).await
}

pub async fn download_dataitem_binary_fast(
    Path(dataitem_key): Path<String>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    resolve_dataitem_impl(dataitem_key, None, true, params, headers, state, true, false).await
}
pub async fn resolve_protected_dataitem(
    Path((pay_to, dataitem_key, network, amount)): Path<(String, String, String, String)>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    let sidecar_config =
        SidecarConfig::load_env(false).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    // 402 payment requirements for this specific request
    let payee = EvmAddress::from_str(&pay_to).map_err(|_| StatusCode::BAD_REQUEST)?;
    let payee_amount = amount.parse::<f64>().unwrap_or(0.01);
    let x402_network =
        Network402104::get_x402_network(&network).map_err(|_| StatusCode::BAD_REQUEST)?;
    let usdc_deployment = x402_network
        .usdc_deployment
        .pay_to(payee)
        .amount(payee_amount)
        .unwrap();
    // default to localhost and fallback to external service url for server's cloud compatibility
    let base_url = get_env_var("EXTERNAL_URL").unwrap_or(format!(
        "http://{}:{}",
        sidecar_config.base_url, sidecar_config.port
    ));

    let payee_info = Payee402 {
        address_str: pay_to.clone(),
        address_primitive: payee,
        amount: payee_amount,
    };

    let payment_requirements = vec![PaymentRequirements {
        scheme: Scheme::Exact,
        network: x402_network.network,
        max_amount_required: usdc_deployment.amount,
        resource: format!("{base_url}/protected/{pay_to}/{dataitem_key}")
            .parse()
            .unwrap(),
        description: "premium xANS-104 dataitem access".to_string(),
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
        payment_requirements: Arc::new(payment_requirements.clone()),
    };

    // payment verification
    let payment_payload = paygate
        .extract_payment_payload(&headers)
        .await
        .map_err(|_| StatusCode::PAYMENT_REQUIRED)?;

    let verify_request = paygate
        .verify_payment(payment_payload.clone())
        .await
        .map_err(|_| StatusCode::PAYMENT_REQUIRED)?;

    let response = resolve_dataitem_impl(
        format!("{dataitem_key}.ans104"),
        Some(payee_info),
        false,
        params,
        headers.clone(),
        state.clone(),
        false,
        false
    )
    .await?;

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
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?,
    );

    Ok(final_response)
}

pub async fn resolve_dataitem_impl(
    dataitem_key: String,
    payee_info: Option<Payee402>,
    send_raw: bool,
    params: ResolveQuery,
    headers: HeaderMap,
    state: AppState,
    is_fast: bool,
    allow_manifest: bool
) -> Result<Response, StatusCode> {
    let s3_client = if is_fast {state.clone().s3_client_fast} else {state.clone().s3_client_normal};
    let mut resolved_key = dataitem_key;
    let mut manifest_origin: Option<String> = None;

    if allow_manifest && params.token.is_none() && payee_info.is_none() {
        if let Some((manifest_id, manifest_path)) = split_manifest_path(&resolved_key) {
            let target_id =
                resolve_manifest_path(&s3_client, &manifest_id, &manifest_path).await?;
            if let Some(target_id) = target_id {
                manifest_origin = Some(manifest_id);
                resolved_key = target_id;
            } else {
                return Err(StatusCode::NOT_FOUND);
            }
        } else if let Some(target_id) =
            resolve_manifest_path(&s3_client, &resolved_key, "").await?
        {
            manifest_origin = Some(resolved_key.clone());
            resolved_key = target_id;
        }
    }

    println!("Resolving dataitem key: {resolved_key}");

    let range_str = params
        .range
        .or_else(|| {
            headers
                .get("range")
                .and_then(|h| h.to_str().ok().map(String::from))
        })
        .unwrap_or_default();

    // determine bucket and key based on jwt token presence
    let (bucket_name, key, payee_address, payee_amount) = if let Some(token) = params.token {
        // private dataitem
        let claims = match crate::sidecar::jwt::validate_dataitem_token(&token, &resolved_key) {
            Ok(claims) => claims,
            Err(_) => {
                return Ok(Response::builder()
                    .status(StatusCode::UNAUTHORIZED)
                    .header("content-type", "application/json")
                    .body(
                        serde_json::to_string(&json!({"error": "jwt token invalid or expired"}))
                            .unwrap()
                            .into(),
                    )
                    .unwrap());
            }
        };

        let bucket = claims.bucket_name;
        (bucket, claims.dataitem_key, claims.payee, claims.amount)
    } else {
        // public dataitem
        let bucket = DATAITEMS_BUCKET.to_string();
        let key = format!("{DATAITEMS_DIR}/{resolved_key}.ans104");
        (bucket, key, None, None)
    };

    // validate if the request has 402 data in the JWT token but no Payee402 is provided
    if payee_address.is_some() && payee_amount.is_some() && payee_info.is_none() {
        return Ok(Response::builder()
                    .status(StatusCode::UNAUTHORIZED)
                    .header("content-type", "application/json")
                    .body(
                        serde_json::to_string(
                            &json!({"error": "request is missing x402 Payee402 struct, use /protected/resolve/{payee}/{*dataitem_key}/{*amount} instead"}),
                        )
                        .unwrap()
                        .into(),
                    )
                    .unwrap());
    }

    if payee_info.is_some() {
        let receipt_402 = payee_info.ok_or(StatusCode::INTERNAL_SERVER_ERROR)?;
        if payee_address.unwrap_or_default() != receipt_402.address_str
            || payee_amount.unwrap_or_default() != receipt_402.amount
        {
            return Ok(Response::builder()
                    .status(StatusCode::UNAUTHORIZED)
                    .header("content-type", "application/json")
                    .body(
                        serde_json::to_string(
                            &json!({"error": "mistmatch between Payee402 receipt and the provided x402 micropayment params"}),
                        )
                        .unwrap()
                        .into(),
                    )
                    .unwrap());
        }
    }

    if send_raw {
        let obj = get_object(&s3_client, &bucket_name, &key, "bytes=0-")
            .await
            .map_err(|_| StatusCode::NOT_FOUND)?;
        let content_length = obj.content_length().unwrap_or(0) as u64;
        let body = Body::from_stream(
            ReaderStream::new(obj.body.into_async_read()).map_err(std::io::Error::other),
        );
        return Response::builder()
            .header("content-type", "application/octet-stream")
            .header("content-length", content_length.to_string())
            .header("access-control-allow-headers", "*")
            .header("access-control-allow-methods", "GET, OPTIONS")
            .body(body)
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR);
    }

    let header_obj = get_object(&s3_client, &bucket_name, &key, "bytes=0-2047")
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
        if allow_manifest && !send_raw {
            if let Some(manifest_id) = manifest_origin {
                if mime_type == "text/html" {
                    let payload =
                        load_dataitem_payload(&s3_client, &bucket_name, &key).await?;
                    let html = rewrite_manifest_html(
                        String::from_utf8_lossy(&payload),
                        &manifest_id,
                    );
                    let body = Body::from(html.clone());
                    return Response::builder()
                        .header("content-type", mime_type)
                        .header("content-length", html.len().to_string())
                        .header("access-control-allow-origin", "*")
                        .header("access-control-allow-headers", "*")
                        .header("access-control-allow-methods", "GET, OPTIONS")
                        .body(body)
                        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR);
                }
            }
        }
        stream_data_section(
            &s3_client,
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
            &s3_client,
            &key,
            &bucket_name,
            data_offset,
            Some((start, end_opt)),
            &mime_type,
        )
        .await
    }
}

fn split_manifest_path(path: &str) -> Option<(String, String)> {
    let mut parts = path.splitn(2, '/');
    let manifest_id = parts.next()?;
    let rest = parts.next()?;
    Some((manifest_id.to_string(), rest.to_string()))
}

async fn resolve_manifest_path(
    client: &aws_sdk_s3::Client,
    manifest_id: &str,
    manifest_path: &str,
) -> Result<Option<String>, StatusCode> {
    let bucket_name = DATAITEMS_BUCKET.to_string();
    let key = format!("{DATAITEMS_DIR}/{manifest_id}.ans104");
    let payload = load_dataitem_payload(client, &bucket_name, &key).await?;

    let manifest: Value =
        serde_json::from_slice(&payload).map_err(|_| StatusCode::NOT_FOUND)?;

    if manifest
        .get("manifest")
        .and_then(|v| v.as_str())
        .filter(|v| *v == "arweave/paths")
        .is_none()
    {
        return Ok(None);
    }

    let paths = manifest
        .get("paths")
        .and_then(|v| v.as_object())
        .ok_or(StatusCode::NOT_FOUND)?;

    let requested_path = manifest_path.trim_start_matches('/');

    let resolve_path_id = |path: &str| -> Option<String> {
        paths
            .get(path)
            .and_then(|v| v.get("id"))
            .and_then(|v| v.as_str())
            .map(str::to_string)
    };

    if requested_path.is_empty() {
        if let Some(index_path) = manifest
            .get("index")
            .and_then(|v| v.get("path"))
            .and_then(|v| v.as_str())
        {
            if let Some(id) = resolve_path_id(index_path) {
                return Ok(Some(id));
            }
        }
    } else if let Some(id) = resolve_path_id(requested_path) {
        return Ok(Some(id));
    }

    let fallback_id = manifest
        .get("fallback")
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .map(str::to_string);

    Ok(fallback_id)
}

async fn load_dataitem_payload(
    client: &aws_sdk_s3::Client,
    bucket_name: &str,
    key: &str,
) -> Result<Vec<u8>, StatusCode> {
    let header_obj = get_object(client, bucket_name, key, "bytes=0-2047")
        .await
        .map_err(|_| StatusCode::NOT_FOUND)?;

    let header_bytes = header_obj
        .body
        .collect()
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
        .into_bytes()
        .to_vec();

    let (_mime_type, data_offset) =
        ans104::parse_ans104_header(&header_bytes).map_err(|_| StatusCode::NOT_FOUND)?;

    let data_range = format!("bytes={data_offset}-");
    let obj = get_object(client, bucket_name, key, &data_range)
        .await
        .map_err(|_| StatusCode::NOT_FOUND)?;

    let bytes = obj
        .body
        .collect()
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
        .into_bytes()
        .to_vec();

    Ok(bytes)
}

fn rewrite_manifest_html(html: std::borrow::Cow<'_, str>, manifest_id: &str) -> String {
    let prefix = format!("/resolve/preview/{manifest_id}/assets/");
    html.replace("\"/assets/", &format!("\"{prefix}"))
        .replace("'/assets/", &format!("'{prefix}"))
}

pub async fn stream_data_section(
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

// TODO: clean signed url creation, modularity
pub async fn create_signed_url(headers: HeaderMap) -> Result<Json<SignedUrlResponse>, StatusCode> {
    validate_api_key(&headers)?;

    let bucket_name = get_header(&headers, "x-bucket-name")?;
    let load_acc = get_header(&headers, "x-load-acc")?;
    let dataitem_key = get_header(&headers, "x-dataitem-key")?;
    let expires_minutes = get_header(&headers, "x-expires-minutes")
        .unwrap_or("60".to_string())
        .parse::<i64>()
        .unwrap_or(60);

    let token = create_signed_dataitem_url(
        &bucket_name,
        &load_acc,
        &dataitem_key,
        expires_minutes,
        None,
        None,
        None,
    )
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let signed_url = format!("{SIDECAR_SERVER_ENDPOINT}/resolve/{dataitem_key}?token={token}");

    Ok(Json(SignedUrlResponse { signed_url }))
}

pub async fn create_402_signed_url(headers: HeaderMap) -> Result<String, StatusCode> {
    validate_api_key(&headers)?;

    let bucket_name = get_header(&headers, "x-bucket-name")?;
    let load_acc = get_header(&headers, "x-load-acc")?;
    let dataitem_key = get_header(&headers, "x-dataitem-key")?;
    let expires_minutes = get_header(&headers, "x-expires-minutes")
        .unwrap_or("60".to_string())
        .parse::<i64>()
        .unwrap_or(60);

    let network_402 = get_header(&headers, "x-402-network").ok();
    let payee_402 = get_header(&headers, "x-402-address").ok();
    let amount_402 = get_header(&headers, "x-402-amount")
        .ok()
        .and_then(|s| s.parse::<f64>().ok())
        .filter(|&amount| amount > 0.0);

    let (payee_opt, amount_opt, network_opt) = match (payee_402, amount_402, network_402) {
        (Some(p), Some(a), Some(n)) if !p.is_empty() => (Some(p), Some(a), Some(n)),
        _ => (None, None, None),
    };

    let token = create_signed_dataitem_url(
        &bucket_name,
        &load_acc,
        &dataitem_key,
        expires_minutes,
        payee_opt.clone(),
        amount_opt,
        network_opt.clone(),
    )
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let response = Signed402UrlResponse {
        token,
        dataitem_id: dataitem_key,
        payee_address: payee_opt.unwrap_or_default(),
        amount: amount_opt.unwrap_or_default(),
        network: network_opt.unwrap_or("polygon-amoy".to_string()), // default to polygon-amoy
    };

    let json_bytes =
        serde_json::to_vec(&response).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let base64_string = general_purpose::STANDARD.encode(&json_bytes);

    Ok(base64_string)
}
