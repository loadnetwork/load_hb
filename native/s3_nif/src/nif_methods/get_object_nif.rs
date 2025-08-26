use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
fn get_object(
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
    key: String,
    range: String,
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

        match crate::s3::get_object(&client, &bucket, &key, &range).await {
            Ok(output) => {
                let mut result = HashMap::new();

                let etag = output.e_tag().unwrap_or("").to_string();
                let last_modified = output
                    .last_modified()
                    .map(|dt| {
                        dt.fmt(aws_smithy_types::date_time::Format::HttpDate)
                            .unwrap()
                    })
                    .unwrap_or_else(|| "".to_string());
                let content_type = output
                    .content_type()
                    .unwrap_or("binary/octet-stream")
                    .to_string();
                let content_length = output.content_length().unwrap_or(0);

                let body_bytes = output.body.collect().await.unwrap().into_bytes();

                // Insert all the data
                result.insert("body".to_string(), body_bytes.to_vec());
                result.insert("etag".to_string(), etag.as_bytes().to_vec());
                result.insert(
                    "last_modified".to_string(),
                    last_modified.as_bytes().to_vec(),
                );
                result.insert("content_type".to_string(), content_type.as_bytes().to_vec());
                result.insert(
                    "content_length".to_string(),
                    content_length.to_string().as_bytes().to_vec(),
                );

                Ok(result)
            }
            Err(e) => Err(format!("Error: {}", e)),
        }
    })
}
