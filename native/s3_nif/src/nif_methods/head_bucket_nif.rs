use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
pub fn head_bucket(
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
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

        match crate::s3::head_bucket(&client, bucket).await {
            Ok(output) => {
                let mut result = HashMap::new();
                let bucket_arn = output.bucket_arn().unwrap_or("").to_string();
                let bucket_location_type = output
                    .bucket_location_type()
                    .map(|blt| blt.as_str())
                    .unwrap_or("")
                    .to_string();
                let bucket_location_name = output.bucket_location_name().unwrap_or("").to_string();
                let bucket_region = output.bucket_region().unwrap_or("").to_string();
                let access_point_alias = output.access_point_alias().unwrap_or(false).to_string();

                result.insert("bucket_arn".to_string(), bucket_arn.as_bytes().to_vec());
                result.insert(
                    "bucket_location_type".to_string(),
                    bucket_location_type.as_bytes().to_vec(),
                );
                result.insert(
                    "bucket_location_name".to_string(),
                    bucket_location_name.as_bytes().to_vec(),
                );
                result.insert(
                    "bucket_region".to_string(),
                    bucket_region.as_bytes().to_vec(),
                );
                result.insert(
                    "access_point_alias".to_string(),
                    access_point_alias.as_bytes().to_vec(),
                );

                Ok(result)
            }

            Err(e) => Err(format!("Error: {}", e)),
        }
    })
}
