use aws_sdk_s3::Client;
use aws_sdk_s3::operation::get_object::GetObjectOutput;

pub async fn retrieve_object(
    client: &Client,
    bucket_name: &str,
    key: &str,
) -> Result<GetObjectOutput, aws_sdk_s3::Error> {
    let resp = client
        .get_object()
        .bucket(bucket_name)
        .key(key)
        .send()
        .await?;

    Ok(resp)
}