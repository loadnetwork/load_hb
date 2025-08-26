use aws_sdk_s3::{Client, operation::head_bucket::HeadBucketOutput};

// TODO: add support for the optional expected_bucket_owner param
pub async fn head_bucket(
    client: &Client,
    bucket: &str,
) -> Result<HeadBucketOutput, aws_sdk_s3::Error> {
    let result = client.head_bucket().bucket(bucket).send().await?;
    Ok(result)
}
