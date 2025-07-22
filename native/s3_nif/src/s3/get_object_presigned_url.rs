use aws_sdk_s3::Client;
use aws_sdk_s3::presigning::PresigningConfig;
use aws_smithy_types::error::operation::BuildError;
use std::time::Duration;

pub async fn generate_get_presigned_url(
    client: &Client,
    bucket: &str,
    key: &str,
    duration: u64,
) -> Result<String, aws_sdk_s3::Error> {
    let request = client.get_object().bucket(bucket).key(key);
    let presigning_config = PresigningConfig::builder()
        .expires_in(Duration::from_secs(duration))
        .build()
        .map_err(|e| (aws_sdk_s3::Error::from(BuildError::other(e.to_string()))))?;
    let presigned_request = request.presigned(presigning_config).await?;
    Ok(presigned_request.uri().to_string())
}
