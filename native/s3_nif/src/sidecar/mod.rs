pub mod ans104;
pub mod handlers;
pub mod http;
pub mod jwt;
pub mod range;
pub mod tags;
pub mod x402104;

use std::sync::Arc;

use anyhow::{Error, anyhow};
use aws_sdk_s3::Client;
use x402_axum::facilitator_client::FacilitatorClient;

pub const SIDECAR_SERVER_ENDPOINT: &str = "https://gateway.s3-node-1.load.network";
pub const FACILITATOR_URL: &str = "https://x402.load.network";
pub const ARWEAVE_GATEWAY: &str = "https://arweave.net";
/// signature verifier for EIP-6492, EIP-1271, EOA, universally deployed on the supported EVM chains
pub const VERIFIER_CONTRACT: &str = "0x41E94Eb019C0762f9Bfcf9Fb1E58725BfB0e7582";

#[derive(Clone)]
pub struct AppState {
    pub s3_client_normal: Client,
    pub s3_client_fast: Client,
    pub x402_facilitator: Arc<FacilitatorClient>,
}

pub async fn run() -> Result<(), Error> {
    http::serve().await.map_err(|e| anyhow!(e.to_string()))
}

pub fn get_env_var(key: &str) -> Result<String, Error> {
    std::env::var(key).map_err(|e| anyhow!(e.to_string()))
}
