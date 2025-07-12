// native/s3_nif/src/lib.rs
pub mod config;
pub mod s3;
pub mod server;
use std::collections::HashMap;

use rustler::NifResult;

#[rustler::nif(schedule = "DirtyCpu")]
fn put_object(
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    bucket: String,
    key: String,
    body: Vec<u8>,
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
        let body_stream = aws_sdk_s3::primitives::ByteStream::from(body);

        match crate::s3::push_object(&client, &bucket, &key, body_stream).await {
            Ok(output) => {
                let mut result = HashMap::new();

                let expiration = output.expiration().unwrap_or("").to_string();
                let etag = output.e_tag().unwrap_or("").to_string();
                let version_id = output.version_id().unwrap_or("").to_string();
                let checksum_crc32 = output.checksum_crc32().unwrap_or("").to_string();
                let checksum_crc32_c = output.checksum_crc32_c().unwrap_or("").to_string();
                let checksum_crc64_nvme = output.checksum_crc64_nvme().unwrap_or("").to_string();
                let checksum_sha1 = output.checksum_sha1().unwrap_or("").to_string();
                let checksum_sha256 = output.checksum_sha256().unwrap_or("").to_string();
                let checksum_type = output
                    .checksum_type()
                    .map(|ct| ct.as_str())
                    .unwrap_or("")
                    .to_string();

                let server_side_encryption = output
                    .server_side_encryption()
                    .map(|sse| sse.as_str())
                    .unwrap_or("")
                    .to_string();
                let sse_customer_algorithm =
                    output.sse_customer_algorithm().unwrap_or("").to_string();
                let sse_customer_key_md5 = output.sse_customer_key_md5().unwrap_or("").to_string();
                let ssekms_key_id = output.ssekms_key_id().unwrap_or("").to_string();
                let ssekms_encryption_context =
                    output.ssekms_encryption_context().unwrap_or("").to_string();
                let bucket_key_enabled = output.bucket_key_enabled().unwrap_or(false).to_string();

                let size = output.size().unwrap_or(0).to_string();
                let request_charged = output
                    .request_charged()
                    .map(|rc| rc.as_str())
                    .unwrap_or("")
                    .to_string();

                // insert all fields
                result.insert("expiration".to_string(), expiration.as_bytes().to_vec());
                result.insert("etag".to_string(), etag.as_bytes().to_vec());
                result.insert("version_id".to_string(), version_id.as_bytes().to_vec());
                result.insert(
                    "checksum_crc32".to_string(),
                    checksum_crc32.as_bytes().to_vec(),
                );
                result.insert(
                    "checksum_crc32_c".to_string(),
                    checksum_crc32_c.as_bytes().to_vec(),
                );
                result.insert(
                    "checksum_crc64_nvme".to_string(),
                    checksum_crc64_nvme.as_bytes().to_vec(),
                );
                result.insert(
                    "checksum_sha1".to_string(),
                    checksum_sha1.as_bytes().to_vec(),
                );
                result.insert(
                    "checksum_sha256".to_string(),
                    checksum_sha256.as_bytes().to_vec(),
                );
                result.insert(
                    "checksum_type".to_string(),
                    checksum_type.as_bytes().to_vec(),
                );
                result.insert(
                    "server_side_encryption".to_string(),
                    server_side_encryption.as_bytes().to_vec(),
                );
                result.insert(
                    "sse_customer_algorithm".to_string(),
                    sse_customer_algorithm.as_bytes().to_vec(),
                );
                result.insert(
                    "sse_customer_key_md5".to_string(),
                    sse_customer_key_md5.as_bytes().to_vec(),
                );
                result.insert(
                    "ssekms_key_id".to_string(),
                    ssekms_key_id.as_bytes().to_vec(),
                );
                result.insert(
                    "ssekms_encryption_context".to_string(),
                    ssekms_encryption_context.as_bytes().to_vec(),
                );
                result.insert(
                    "bucket_key_enabled".to_string(),
                    bucket_key_enabled.as_bytes().to_vec(),
                );
                result.insert("size".to_string(), size.as_bytes().to_vec());
                result.insert(
                    "request_charged".to_string(),
                    request_charged.as_bytes().to_vec(),
                );

                Ok(result)
            }

            Err(e) => Err(format!("S3 Error {}", e)),
        }
    })
}

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
        let client = crate::s3::create_s3_client(
            &endpoint,
            &access_key_id,
            &secret_access_key,
            &region,
            Some(true),
        )
        .await;

        match crate::s3::retrieve_object(&client, &bucket, &key).await {
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
            Err(e) => Err(format!("S3 Error: {}", e)),
        }
    })
}

rustler::init!("s3_nif", [get_object, put_object]);
