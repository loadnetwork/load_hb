use s3_nif::config::S3Config;
use s3_nif::server::S3Server;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = S3Config::from_env();
    let server = S3Server::from_config(config).await?;
    server.serve().await?;
    Ok(())
}