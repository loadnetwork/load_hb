use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
pub fn list_objects(
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
    prefix: String,
    delimiter: String,
    marker: String,
    max_keys: String,
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

        match crate::s3::list_objects(&client, &bucket, &prefix, &delimiter, &marker, &max_keys)
            .await
        {
            Ok(output) => {
                let mut result = HashMap::new();

                let is_truncated = output.is_truncated().unwrap_or(false).to_string();
                let marker = output.marker().unwrap_or("").to_string();
                let next_marker = output.next_marker().unwrap_or("").to_string();
                let name = output.name().unwrap_or("").to_string();
                let prefix = output.prefix().unwrap_or("").to_string();
                let delimiter = output.delimiter().unwrap_or("").to_string();
                let max_keys = output.max_keys().unwrap_or(0).to_string();
                let encoding_type = output
                    .encoding_type()
                    .map(|et| et.as_str())
                    .unwrap_or("")
                    .to_string();
                let request_charged = output
                    .request_charged()
                    .map(|rc| rc.as_str())
                    .unwrap_or("")
                    .to_string();

                result.insert("is_truncated".to_string(), is_truncated.as_bytes().to_vec());
                result.insert("marker".to_string(), marker.as_bytes().to_vec());
                result.insert("next_marker".to_string(), next_marker.as_bytes().to_vec());
                result.insert("name".to_string(), name.as_bytes().to_vec());
                result.insert("prefix".to_string(), prefix.as_bytes().to_vec());
                result.insert("delimiter".to_string(), delimiter.as_bytes().to_vec());
                result.insert("max_keys".to_string(), max_keys.as_bytes().to_vec());
                result.insert(
                    "encoding_type".to_string(),
                    encoding_type.as_bytes().to_vec(),
                );
                result.insert(
                    "request_charged".to_string(),
                    request_charged.as_bytes().to_vec(),
                );

                let contents = output.contents();
                if !contents.is_empty() {
                    for (i, object) in contents.iter().enumerate() {
                        let key = object.key().unwrap_or("");
                        let last_modified = object
                            .last_modified()
                            .map(|dt| {
                                dt.fmt(aws_smithy_types::date_time::Format::HttpDate)
                                    .unwrap()
                            })
                            .unwrap_or_else(|| "".to_string());
                        let etag = object.e_tag().unwrap_or("");
                        let size = object.size().unwrap_or(0).to_string();
                        let storage_class =
                            object.storage_class().map(|sc| sc.as_str()).unwrap_or("");
                        let owner_display_name =
                            object.owner().and_then(|o| o.display_name()).unwrap_or("");
                        let owner_id = object.owner().and_then(|o| o.id()).unwrap_or("");

                        // store each object with indexed keys
                        result.insert(format!("object_{}_key", i), key.as_bytes().to_vec());
                        result.insert(
                            format!("object_{}_last_modified", i),
                            last_modified.as_bytes().to_vec(),
                        );
                        result.insert(format!("object_{}_etag", i), etag.as_bytes().to_vec());
                        result.insert(format!("object_{}_size", i), size.as_bytes().to_vec());
                        result.insert(
                            format!("object_{}_storage_class", i),
                            storage_class.as_bytes().to_vec(),
                        );
                        result.insert(
                            format!("object_{}_owner_display_name", i),
                            owner_display_name.as_bytes().to_vec(),
                        );
                        result.insert(
                            format!("object_{}_owner_id", i),
                            owner_id.as_bytes().to_vec(),
                        );
                    }
                    result.insert(
                        "object_count".to_string(),
                        contents.len().to_string().as_bytes().to_vec(),
                    );
                } else {
                    result.insert("object_count".to_string(), "0".as_bytes().to_vec());
                }

                let common_prefixes = output.common_prefixes();
                if !common_prefixes.is_empty() {
                    for (i, common_prefix) in common_prefixes.iter().enumerate() {
                        let prefix = common_prefix.prefix().unwrap_or("");
                        result.insert(format!("common_prefix_{}", i), prefix.as_bytes().to_vec());
                    }
                    result.insert(
                        "common_prefix_count".to_string(),
                        common_prefixes.len().to_string().as_bytes().to_vec(),
                    );
                } else {
                    result.insert("common_prefix_count".to_string(), "0".as_bytes().to_vec());
                }

                Ok(result)
            }

            Err(e) => Err(format!("S3 List Objects Error: {}", e.to_string())),
        }
    })
}
