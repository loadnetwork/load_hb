pub mod ans104;
pub mod http;
pub mod range;
pub mod tags;
pub mod jwt;

use anyhow::{Error, anyhow};
use aws_sdk_s3::Client;

pub const SIDECAR_SERVER_ENDPOINT: &str = "https://gateway.s3-node-1.load.network";

#[derive(Clone)]
pub struct AppState {
    pub s3_client: Client,
}

pub async fn run() -> Result<(), Error> {
    http::serve().await.map_err(|e| anyhow!(e.to_string()))
}

pub fn get_env_var(key: &str) -> Result<String, Error> {
    std::env::var(key).map_err(|e| anyhow!(e.to_string()))
}
