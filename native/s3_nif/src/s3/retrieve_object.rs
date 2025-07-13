use aws_sdk_s3::Client;
use aws_sdk_s3::operation::get_object::GetObjectOutput;
use aws_sdk_s3::operation::head_object::HeadObjectOutput;

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

pub async fn head_object(
    client: &Client,
    bucket_name: &str,
    key: &str,
) -> Result<HeadObjectOutput, aws_sdk_s3::Error> {
    let res = client
        .head_object()
        .bucket(bucket_name)
        .key(key)
        .send()
        .await?;
    Ok(res)
}
