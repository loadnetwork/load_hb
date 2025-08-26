use crate::s3::put_object_expiry;
use aws_sdk_s3::Client;
use aws_sdk_s3::operation::put_object::PutObjectOutput;
use aws_sdk_s3::primitives::ByteStream;

// expiry handler, passing `0` from erlang dev_s3:put_object_handler is considered as
// using the handler to put object with no expiry

pub async fn put_object(
    client: &Client,
    bucket_name: &str,
    key: &str,
    body: ByteStream,
    expiry_days: i32,
) -> Result<PutObjectOutput, aws_sdk_s3::Error> {
    if expiry_days > 0 {
        let res = put_object_expiry(client, bucket_name, key, body, expiry_days).await?;
        Ok(res)
    } else {
        let res = put_object_no_expiry(client, bucket_name, key, body).await?;
        Ok(res)
    }
}

pub async fn put_object_no_expiry(
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
