use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
pub fn delete_objects(
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
    objects_to_delete: Vec<String>,
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

        match crate::s3::delete_objects(&client, &bucket, objects_to_delete).await {
            Ok(output) => {
                let mut result = HashMap::new();

                // Handle request_charged
                let request_charged = output
                    .request_charged()
                    .map(|rc| rc.as_str())
                    .unwrap_or("")
                    .to_string();
                result.insert(
                    "request_charged".to_string(),
                    request_charged.as_bytes().to_vec(),
                );

                let deleted_objects = output.deleted();

                if !deleted_objects.is_empty() {
                    for (i, deleted_obj) in deleted_objects.iter().enumerate() {
                        let key = deleted_obj.key().unwrap_or("");
                        let version_id = deleted_obj.version_id().unwrap_or("");
                        let delete_marker =
                            deleted_obj.delete_marker().unwrap_or(false).to_string();
                        let delete_marker_version_id =
                            deleted_obj.delete_marker_version_id().unwrap_or("");

                        result.insert(format!("deleted_{}_key", i), key.as_bytes().to_vec());
                        result.insert(
                            format!("deleted_{}_version_id", i),
                            version_id.as_bytes().to_vec(),
                        );
                        result.insert(
                            format!("deleted_{}_delete_marker", i),
                            delete_marker.as_bytes().to_vec(),
                        );
                        result.insert(
                            format!("deleted_{}_delete_marker_version_id", i),
                            delete_marker_version_id.as_bytes().to_vec(),
                        );
                    }
                    result.insert(
                        "deleted_count".to_string(),
                        deleted_objects.len().to_string().as_bytes().to_vec(),
                    );
                } else {
                    result.insert("deleted_count".to_string(), "0".as_bytes().to_vec());
                }

                let errors = output.errors();
                if !errors.is_empty() {
                    for (i, error) in errors.iter().enumerate() {
                        let key = error.key().unwrap_or("");
                        let code = error.code().unwrap_or("");
                        let message = error.message().unwrap_or("");
                        let version_id = error.version_id().unwrap_or("");

                        result.insert(format!("error_{}_key", i), key.as_bytes().to_vec());
                        result.insert(format!("error_{}_code", i), code.as_bytes().to_vec());
                        result.insert(format!("error_{}_message", i), message.as_bytes().to_vec());
                        result.insert(
                            format!("error_{}_version_id", i),
                            version_id.as_bytes().to_vec(),
                        );
                    }
                    result.insert(
                        "error_count".to_string(),
                        errors.len().to_string().as_bytes().to_vec(),
                    );
                } else {
                    result.insert("error_count".to_string(), "0".as_bytes().to_vec());
                }

                Ok(result)
            }

            Err(e) => Err(format!("Error: {}", e)),
        }
    })
}
