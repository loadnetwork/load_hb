use anyhow::{Error, anyhow};
use x402_rs::network::{Network, USDCDeployment};

const SUPPORTED_NETWORKS: [&str; 3] = ["base", "base-sepolia", "polygon-amoy"];

#[derive(Debug, Clone)]
pub struct Network402104 {
    pub network: Network,
    pub usdc_deployment: USDCDeployment,
}

impl Network402104 {
    pub fn get_x402_network(network: &str) -> Result<Self, Error> {
        if !SUPPORTED_NETWORKS.contains(&network) {
            return Err(anyhow!("error {network} key is not supported"));
        }

        let network_key = format!("\"{network}\"");

        let network: Network = serde_json::from_str(&network_key)?;
        Ok(Self {
            network,
            usdc_deployment: USDCDeployment::by_network(network).clone(),
        })
    }
}
