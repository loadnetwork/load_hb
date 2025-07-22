use crate::s3::{create_s3_client, generate_get_presigned_url};

#[rustler::nif(schedule = "DirtyCpu")]
pub fn presigned_get_object(
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
    key: String,
    duration: u64,
) -> Result<String, String> {
    let rt = tokio::runtime::Runtime::new().map_err(|e| e.to_string())?;
    rt.block_on(async {
        let client = create_s3_client(
            &endpoint,
            &access_key_id,
            &secret_access_key,
            &region,
            Some(true),
        )
        .await;
        let presigned_url = generate_get_presigned_url(&client, &bucket, &key, duration)
            .await
            .map_err(|e| e.to_string())?;
        Ok(presigned_url)
    })
}
