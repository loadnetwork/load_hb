use anyhow::Error;
use axum::{
    Router,
    http::{HeaderMap, StatusCode},
    routing::{get, post},
};
use serde::Serialize;
use std::sync::Arc;
use tower_http::cors::CorsLayer;
use x402_axum::facilitator_client::FacilitatorClient;
use x402_rs::types::EvmAddress;

use crate::s3::create_s3_client;
use crate::sidecar::{AppState, FACILITATOR_URL, get_env_var};

use crate::sidecar::handlers::{
    create_402_signed_url, create_signed_url, download_dataitem_binary, resolve_dataitem_normal,
    resolve_protected_dataitem, root, resolve_dataitem_fast, download_dataitem_binary_fast,
    resolve_dataitem_preview, resolve_dataitem_preview_fast
};

#[derive(serde::Deserialize)]
pub struct ResolveQuery {
    pub range: Option<String>,
    pub token: Option<String>,
}

#[derive(Debug)]
pub(crate) struct SidecarConfig {
    pub endpoint: String,
    pub access_key_id: String,
    pub secret_access_key: String,
    pub region: String,
    pub port: String,
    pub(crate) jwk_priv: String,
    pub base_url: String,
}

#[derive(Serialize)]
pub struct SignedUrlResponse {
    pub signed_url: String,
}

#[derive(Serialize)]
pub(crate) struct Signed402UrlResponse {
    pub token: String,
    pub dataitem_id: String,
    pub payee_address: String,
    pub amount: f64,
    pub network: String,
}

#[derive(Debug, Clone)]
pub struct Payee402 {
    pub address_str: String,
    pub address_primitive: EvmAddress,
    pub amount: f64,
}

impl SidecarConfig {
    pub fn load_env(is_fast: bool) -> Result<Self, Error> {
        let suffix = if is_fast {"_FAST".to_string()} else {"".to_string()};
        Ok(Self {
            endpoint: get_env_var(&format!("ENDPOINT{suffix}"))?,
            access_key_id: get_env_var(&format!("ACCESS_KEY_ID{suffix}"))?,
            secret_access_key: get_env_var(&format!("SECRET_ACCESS_KEY{suffix}"))?,
            region: get_env_var(&format!("REGION{suffix}"))?,
            port: get_env_var("PORT")?,
            jwk_priv: get_env_var("PRESIGNED_URL_JWT_PRIV")?,
            base_url: get_env_var("BASE_URL")?,
        })
    }
}

pub fn get_header(headers: &HeaderMap, name: &str) -> Result<String, StatusCode> {
    headers
        .get(name)
        .and_then(|h| h.to_str().ok())
        .map(String::from)
        .ok_or(StatusCode::BAD_REQUEST)
}

pub fn validate_api_key(headers: &HeaderMap) -> Result<(), StatusCode> {
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
    let sidecar_config = SidecarConfig::load_env(false)?;
    let sidecar_config_fast = SidecarConfig::load_env(true)?;

    println!(
        "offchain ANS-104 streaming sidecar v{}",
        env!("CARGO_PKG_VERSION")
    );
    println!("running on port {}", sidecar_config.port);

    let s3_client_normal = create_s3_client(
        &sidecar_config.endpoint,
        &sidecar_config.access_key_id,
        &sidecar_config.secret_access_key,
        &sidecar_config.region,
        Some(true),
    )
    .await;

    let s3_client_fast = create_s3_client(
        &sidecar_config_fast.endpoint,
        &sidecar_config_fast.access_key_id,
        &sidecar_config_fast.secret_access_key,
        &sidecar_config_fast.region,
        Some(true),
    )
    .await;

    let x402_facilitator = Arc::new(FacilitatorClient::try_from(FACILITATOR_URL)?);

    let app_state = AppState {
        s3_client_normal,
        s3_client_fast,
        x402_facilitator,
    };

    let app = Router::new()
        .route("/", get(root))
        .route("/resolve/{*dataitem_key}", get(resolve_dataitem_normal))
        .route("/resolve/fast/{*dataitem_key}", get(resolve_dataitem_fast))
        .route("/resolve/preview/{*dataitem_key}", get(resolve_dataitem_preview))
        .route("/resolve/preview/fast/{*dataitem_key}", get(resolve_dataitem_preview_fast))
        .route("/binary/{dataitem_key}", get(download_dataitem_binary))
        .route("/binary/fast/{dataitem_key}", get(download_dataitem_binary_fast))
        .route("/health", get(|| async { "sidecar running" }))
        .route("/sign", post(create_signed_url))
        .route("/sign/402", post(create_402_signed_url))
        .route(
            "/protected/resolve/{payee}/{dataitem_key}/{network}/{*amount}",
            get(resolve_protected_dataitem),
        )
        .layer(CorsLayer::permissive())
        .with_state(app_state);

    let listener = tokio::net::TcpListener::bind(format!(
        "{}:{}",
        sidecar_config.base_url, sidecar_config.port
    ))
    .await?;

    axum::serve(listener, app).await?;
    Ok(())
}
