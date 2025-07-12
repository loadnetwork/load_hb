use aws_sdk_s3::Client;

pub async fn create_bucket(client: &Client, bucket_name: &str) -> Result<(), aws_sdk_s3::Error> {
    client.create_bucket().bucket(bucket_name).send().await?;

    Ok(())
}
