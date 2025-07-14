use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
pub fn list_all_objects(
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
    prefix: String,
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

        match crate::s3::list_all_objects(&client, &bucket, &prefix).await {
            Ok(all_objects) => {
                let mut result = HashMap::new();

                // no pagination, iterate and store all objects
                for (i, object) in all_objects.iter().enumerate() {
                    let key = object.key().unwrap_or("");
                    let size = object.size().unwrap_or(0).to_string();
                    let etag = object.e_tag().unwrap_or("");

                    result.insert(format!("object_{}_key", i), key.as_bytes().to_vec());
                    result.insert(format!("object_{}_size", i), size.as_bytes().to_vec());
                    result.insert(format!("object_{}_etag", i), etag.as_bytes().to_vec());
                }

                result.insert(
                    "object_count".to_string(),
                    all_objects.len().to_string().as_bytes().to_vec(),
                );
                result.insert("is_complete".to_string(), "true".as_bytes().to_vec());

                Ok(result)
            }
            Err(e) => Err(format!("Error: {}", e)),
        }
    })
}
