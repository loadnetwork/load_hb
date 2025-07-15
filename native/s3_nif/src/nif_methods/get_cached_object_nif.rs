use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
fn get_cached_object(
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
    key: String,
) -> Result<HashMap<String, Vec<u8>>, String> {
    let rt = tokio::runtime::Runtime::new().map_err(|e| e.to_string())?;

    rt.block_on(async {
        let client = crate::s3::create_s3_client(
            &endpoint,
            &access_key_id,
            &secret_access_key,
            &region,
            Some(true),
        )
        .await;

        match crate::s3::get_object_with_metadata(&client, &bucket, &key).await {
            Ok(output) => {
                let mut result = HashMap::new();

                let etag = output.1.unwrap_or_default().to_string();
                let body_bytes = output.0;
                result.insert("body".to_string(), body_bytes);
                result.insert("etag".to_string(), etag.as_bytes().to_vec());

                Ok(result)
            }
            Err(_) => Err(format!("Error: failed to retrieve cached object")),
        }
    })
}
