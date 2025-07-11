use aws_sdk_s3::Client;
use aws_sdk_s3::operation::put_object::PutObjectOutput;
use aws_sdk_s3::primitives::ByteStream;

pub async fn push_object(
    client: &Client,
    bucket_name: &str,
    key: &str,
    body: ByteStream,
) -> Result<PutObjectOutput, aws_sdk_s3::Error> {
    let response = client
        .put_object()
        .bucket(bucket_name)
        .key(key)
        .body(body)
        .send()
        .await?;

    Ok(response)
}