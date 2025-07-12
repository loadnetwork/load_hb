pub mod config;
pub mod s3;
pub mod server;

use aws_config::BehaviorVersion;
use aws_sdk_s3::{Client, config::Region};

// native/s3_nif/src/lib.rs
use rustler::{Env, Term, NifResult, Encoder, Decoder, Binary};
use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
fn get_object(
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
    key: String,
) -> Result<HashMap<String, Vec<u8>>, String> {
    println!("get_object called from rust nif");
    let rt = tokio::runtime::Runtime::new().map_err(|e| e.to_string())?;
    
    rt.block_on(async {
        let client = crate::create_s3_client(
            &endpoint,
            &access_key_id,
            &secret_access_key,
            &region,
            Some(true),
        ).await;

        println!("CLIENT CREATED {:?}", client);

        // Use your existing retrieve_object function
match crate::s3::retrieve_object(&client, &bucket, &key).await {
    Ok(output) => {
        let mut result = HashMap::new();
        
        // Extract metadata FIRST (before consuming the body)
        let etag = output.e_tag().unwrap_or("").to_string();
        let last_modified = output.last_modified()
            .map(|dt| dt.fmt(aws_smithy_types::date_time::Format::HttpDate).unwrap())
            .unwrap_or_else(|| "".to_string());
        let content_type = output.content_type().unwrap_or("binary/octet-stream").to_string();
        let content_length = output.content_length().unwrap_or(0);
        
        // NOW consume the body (this moves output.body)
        let body_bytes = output.body.collect().await.unwrap().into_bytes();
        
        // Insert all the data
        result.insert("body".to_string(), body_bytes.to_vec());
        result.insert("etag".to_string(), etag.as_bytes().to_vec());
        result.insert("last_modified".to_string(), last_modified.as_bytes().to_vec());
        result.insert("content_type".to_string(), content_type.as_bytes().to_vec());
        result.insert("content_length".to_string(), content_length.to_string().as_bytes().to_vec());
        
        Ok(result)
    }
    Err(e) => Err(format!("S3 Error: {}", e))
}

    })
}


pub async fn create_s3_client(
    endpoint_url: &str,
    access_key: &str,
    secret_key: &str,
    region: &str,
    force_path_style: Option<bool>,
) -> Client {
    let force_path_style = force_path_style.unwrap_or(true);

    let config = aws_config::defaults(BehaviorVersion::latest())
        .region(Region::new(region.to_string()))
        .endpoint_url(endpoint_url)
        .credentials_provider(aws_sdk_s3::config::Credentials::new(
            access_key, secret_key, None, None, "static",
        ))
        .load()
        .await;

    let mut s3_config = aws_sdk_s3::config::Builder::from(&config);
    if force_path_style {
        s3_config = s3_config.force_path_style(true);
    }

    Client::from_conf(s3_config.build())
}

rustler::init!("s3_nif", [get_object]);

