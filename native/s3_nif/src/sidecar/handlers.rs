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
use x402_rs::network::{Network, USDCDeployment};
use x402_rs::types::{Base64Bytes, EvmAddress, PaymentRequirements, Scheme};

use crate::sidecar::jwt::create_signed_dataitem_url;

use crate::s3::{DATAITEMS_BUCKET, DATAITEMS_DIR, get_object};
use crate::sidecar::{
    AppState, FACILITATOR_URL, SIDECAR_SERVER_ENDPOINT, ans104, get_env_var, range,
};

use crate::sidecar::http::{
    Payee402, ResolveQuery, SidecarConfig, Signed402UrlResponse, SignedUrlResponse, get_header,
    validate_api_key,
};

pub async fn root() -> Json<Value> {
    return Json(json!({
        "version": env!("CARGO_PKG_VERSION"),
        "running": true,
        "x402": true,
        "private-dataitems": true,
        "x402_facilitator": FACILITATOR_URL,
        "hb_node": "s3-node-1.load.network"
    }));
}

pub async fn resolve_dataitem(
    Path(dataitem_key): Path<String>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    resolve_dataitem_impl(dataitem_key, None, params, headers, state).await
}

pub async fn resolve_protected_dataitem(
    Path((pay_to, dataitem_key, amount)): Path<(String, String, String)>,
    Query(params): Query<ResolveQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    let sidecar_config =
        SidecarConfig::load_env().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    // 402 payment requirements for this specific request
    let payee = EvmAddress::from_str(&pay_to).map_err(|_| StatusCode::BAD_REQUEST)?;
    let payee_amount = amount.parse::<f64>().unwrap_or(0.01);
    let usdc_deployment = USDCDeployment::by_network(Network::PolygonAmoy)
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
        network: Network::PolygonAmoy,
        max_amount_required: usdc_deployment.amount,
        resource: format!("{}/protected/{}/{}", base_url, pay_to, dataitem_key)
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

    let response = resolve_dataitem_impl(
        format!("{dataitem_key}.ans104"),
        Some(payee_info),
        params,
        headers.clone(),
        state.clone(),
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
    let (bucket_name, key, payee_address, payee_amount) = if let Some(token) = params.token {
        // private dataitem
        let claims = match crate::sidecar::jwt::validate_dataitem_token(&token, &dataitem_key) {
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
        let key = format!("{DATAITEMS_DIR}/{dataitem_key}.ans104");
        (bucket, key, None, None)
    };

    // println!("resolving with: {bucket_name} {key} {} {}", payee_address.clone().unwrap_or_default(), payee_amount.clone().unwrap_or_default());
    // println!("payee info {:?}", payee_info.clone().unwrap());

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
        let receipt_402 = payee_info.ok_or_else(|| StatusCode::INTERNAL_SERVER_ERROR)?;
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

    // let payee_402 = get_header(&headers, "x-402-address").ok();
    // let amount_402 = get_header(&headers, "x-402-amount")
    //     .ok()
    //     .and_then(|s| s.parse::<f64>().ok())
    //     .filter(|&amount| amount > 0.0);

    // let (payee_opt, amount_opt) = match (payee_402, amount_402) {
    //     (Some(p), Some(a)) if !p.is_empty() => (Some(p), Some(a)),
    //     _ => (None, None),
    // };

    let token = create_signed_dataitem_url(
        &bucket_name,
        &load_acc,
        &dataitem_key,
        expires_minutes,
        // payee_opt,
        // amount_opt,
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

    let payee_402 = get_header(&headers, "x-402-address").ok();
    let amount_402 = get_header(&headers, "x-402-amount")
        .ok()
        .and_then(|s| s.parse::<f64>().ok())
        .filter(|&amount| amount > 0.0);

    let (payee_opt, amount_opt) = match (payee_402, amount_402) {
        (Some(p), Some(a)) if !p.is_empty() => (Some(p), Some(a)),
        _ => (None, None),
    };

    let token = create_signed_dataitem_url(
        &bucket_name,
        &load_acc,
        &dataitem_key,
        expires_minutes,
        payee_opt.clone(),
        amount_opt,
    )
    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let response = Signed402UrlResponse {
        token,
        dataitem_id: dataitem_key,
        payee_address: payee_opt.unwrap_or_default(),
        amount: amount_opt.unwrap_or_default(),
    };

    let json_bytes =
        serde_json::to_vec(&response).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let base64_string = general_purpose::STANDARD.encode(&json_bytes);

    Ok(base64_string)
}
