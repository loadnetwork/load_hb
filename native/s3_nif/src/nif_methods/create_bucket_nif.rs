use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
fn create_bucket(
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

        match crate::s3::create_bucket(&client, &bucket).await {
            Ok(output) => {
                let mut result = HashMap::new();
                let location = output.location().unwrap_or("").to_string();
                let bucket_arn = output.bucket_arn().unwrap_or("").to_string();

                result.insert("location".to_string(), location.as_bytes().to_vec());
                result.insert("bucket_arn".to_string(), bucket_arn.as_bytes().to_vec());

                Ok(result)
            }
            Err(e) => Err(format!("Error: {}", e)),
        }
    })
}
