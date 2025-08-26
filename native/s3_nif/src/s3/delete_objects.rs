use aws_sdk_s3::types::{Delete, ObjectIdentifier};
use aws_sdk_s3::{Client, operation::delete_objects::DeleteObjectsOutput};

pub async fn delete_objects(
    client: &Client,
    bucket: &str,
    objects_to_delete: Vec<String>,
) -> Result<DeleteObjectsOutput, aws_sdk_s3::Error> {
    let objects: Vec<ObjectIdentifier> = objects_to_delete
        .into_iter()
        .map(|key| ObjectIdentifier::builder().key(key).build())
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| aws_sdk_s3::Error::from(e))?;

    let delete_builder = Delete::builder().set_objects(Some(objects));

    let delete_request = delete_builder
        .build()
        .map_err(|e| aws_sdk_s3::Error::from(e))?;

    let result = client
        .delete_objects()
        .bucket(bucket)
        .delete(delete_request)
        .send()
        .await?;

    Ok(result)
}
