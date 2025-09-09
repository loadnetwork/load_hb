use dotenvy::dotenv;
use s3_nif::sidecar::run;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenv().ok();
    run().await
}
