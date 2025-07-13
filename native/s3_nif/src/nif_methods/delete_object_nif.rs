use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
pub fn delete_object(
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

        match crate::s3::delete_object(&client, bucket, key).await {
            Ok(output) => {
                let mut result = HashMap::new();
                let delete_marker = output.delete_marker().unwrap_or(false).to_string();
                let version_id = output.version_id().unwrap_or("").to_string();
                let request_charged = output
                    .request_charged()
                    .map(|rc| rc.as_str())
                    .unwrap_or("")
                    .to_string();

                result.insert(
                    "delete_marker".to_string(),
                    delete_marker.as_bytes().to_vec(),
                );
                result.insert("version_id".to_string(), version_id.as_bytes().to_vec());
                result.insert(
                    "request_charged".to_string(),
                    request_charged.as_bytes().to_vec(),
                );

                Ok(result)
            }

            Err(e) => Err(format!("Error: {}", e)),
        }
    })
}
