use aws_sdk_s3::{Client, operation::delete_object::DeleteObjectOutput};

// TODO: add support for the optional version_id param for wider compatibility
pub async fn delete_object(
    client: &Client,
    bucket: &str,
    key: &str,
) -> Result<DeleteObjectOutput, aws_sdk_s3::Error> {
    let result = client
        .delete_object()
        .bucket(bucket)
        .key(key)
        .send()
        .await?;
    Ok(result)
}
