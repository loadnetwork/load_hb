pub mod ans104;
pub mod handlers;
pub mod http;
pub mod jwt;
pub mod range;
pub mod tags;

use std::sync::Arc;

use anyhow::{Error, anyhow};
use aws_sdk_s3::Client;
use x402_axum::facilitator_client::FacilitatorClient;

pub const SIDECAR_SERVER_ENDPOINT: &str = "https://gateway.s3-node-1.load.network";
pub const FACILITATOR_URL: &str = "https://x402.load.network";

#[derive(Clone)]
pub struct AppState {
    pub s3_client: Client,
    pub x402_facilitator: Arc<FacilitatorClient>,
}

pub async fn run() -> Result<(), Error> {
    http::serve().await.map_err(|e| anyhow!(e.to_string()))
}

pub fn get_env_var(key: &str) -> Result<String, Error> {
    std::env::var(key).map_err(|e| anyhow!(e.to_string()))
}
