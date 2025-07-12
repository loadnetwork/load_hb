use aws_sdk_s3::{Client, operation::create_bucket::CreateBucketOutput};

pub async fn create_bucket(
    client: &Client,
    bucket_name: &str,
) -> Result<CreateBucketOutput, aws_sdk_s3::Error> {
    let res: CreateBucketOutput = client.create_bucket().bucket(bucket_name).send().await?;

    Ok(res)
}
