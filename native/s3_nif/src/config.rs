use aws_config::BehaviorVersion;
use aws_sdk_s3::Client;
use aws_sdk_s3::config::{Builder, Credentials, Region};
use std::env;

#[derive(Debug, Clone)]
pub struct S3Config {
    pub endpoint: String,
    pub region: String,
    pub access_key_id: String,
    pub secret_access_key: String,
    pub port: u16,
    pub force_path_style: bool,
}

impl Default for S3Config {
    fn default() -> Self {
        Self {
            endpoint: "http://localhost:9000".to_string(),
            region: "us-east-1".to_string(),
            access_key_id: "minioadmin".to_string(),
            secret_access_key: "minioadmin".to_string(),
            port: 8000,
            force_path_style: true,
        }
    }
}

impl S3Config {
    pub fn from_env() -> Self {
        Self {
            endpoint: env::var("ENDPOINT").unwrap_or_else(|_| "http://localhost:9000".to_string()),
            region: env::var("AWS_REGION").unwrap_or_else(|_| "us-east-1".to_string()),
            access_key_id: env::var("AWS_ACCESS_KEY_ID")
                .unwrap_or_else(|_| "minioadmin".to_string()),
            secret_access_key: env::var("AWS_SECRET_ACCESS_KEY")
                .unwrap_or_else(|_| "minioadmin".to_string()),
            port: env::var("PORT")
                .unwrap_or_else(|_| "8000".to_string())
                .parse::<u16>()
                .unwrap_or(8000),
            force_path_style: env::var("FORCE_PATH_STYLE")
                .unwrap_or_else(|_| "true".to_string())
                .parse()
                .unwrap_or(true),
        }
    }

    pub fn with_endpoint(mut self, endpoint: String) -> Self {
        self.endpoint = endpoint;
        self
    }

    pub fn with_region(mut self, region: String) -> Self {
        self.region = region;
        self
    }

    pub fn with_credentials(mut self, access_key_id: String, secret_access_key: String) -> Self {
        self.access_key_id = access_key_id;
        self.secret_access_key = secret_access_key;
        self
    }

    pub fn with_port(mut self, port: u16) -> Self {
        self.port = port;
        self
    }

    pub fn with_path_style(mut self, force_path_style: bool) -> Self {
        self.force_path_style = force_path_style;
        self
    }

    pub async fn build_client(&self) -> Result<Client, Box<dyn std::error::Error>> {
        let config = aws_config::load_defaults(BehaviorVersion::latest()).await;

        let credentials = Credentials::new(
            &self.access_key_id,
            &self.secret_access_key,
            None,
            None,
            "s3-hb-mirror",
        );

        let s3_config = Builder::from(&config)
            .region(Region::new(self.region.clone()))
            .endpoint_url(self.endpoint.clone())
            .credentials_provider(credentials)
            .force_path_style(self.force_path_style)
            .build();

        Ok(Client::from_conf(s3_config))
    }
}