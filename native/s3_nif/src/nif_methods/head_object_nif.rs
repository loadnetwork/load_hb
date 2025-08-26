use std::collections::HashMap;

#[rustler::nif(schedule = "DirtyCpu")]
pub fn head_object(
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

        match crate::s3::head_object(&client, &bucket, &key).await {
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
                let accept_ranges = output.accept_ranges().unwrap_or("").to_string();
                let cache_control = output.cache_control().unwrap_or("").to_string();
                let content_disposition = output.content_disposition().unwrap_or("").to_string();
                let content_encoding = output.content_encoding().unwrap_or("").to_string();
                let content_language = output.content_language().unwrap_or("").to_string();
                let content_range = output.content_range().unwrap_or("").to_string();
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
                let version_id = output.version_id().unwrap_or("").to_string();
                let delete_marker = output.delete_marker().unwrap_or(false).to_string();
                let expiration = output.expiration().unwrap_or("").to_string();
                let restore = output.restore().unwrap_or("").to_string();
                let archive_status = output
                    .archive_status()
                    .map(|as_| as_.as_str())
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
                let bucket_key_enabled = output.bucket_key_enabled().unwrap_or(false).to_string();
                let storage_class = output
                    .storage_class()
                    .map(|sc| sc.as_str())
                    .unwrap_or("")
                    .to_string();
                let replication_status = output
                    .replication_status()
                    .map(|rs| rs.as_str())
                    .unwrap_or("")
                    .to_string();
                let request_charged = output
                    .request_charged()
                    .map(|rc| rc.as_str())
                    .unwrap_or("")
                    .to_string();
                let object_lock_mode = output
                    .object_lock_mode()
                    .map(|olm| olm.as_str())
                    .unwrap_or("")
                    .to_string();
                let object_lock_retain_until_date = output
                    .object_lock_retain_until_date()
                    .map(|dt| {
                        dt.fmt(aws_smithy_types::date_time::Format::HttpDate)
                            .unwrap()
                    })
                    .unwrap_or_else(|| "".to_string());
                let object_lock_legal_hold_status = output
                    .object_lock_legal_hold_status()
                    .map(|olhs| olhs.as_str())
                    .unwrap_or("")
                    .to_string();
                let missing_meta = output.missing_meta().unwrap_or(0).to_string();
                let parts_count = output.parts_count().unwrap_or(0).to_string();
                let tag_count = output.tag_count().unwrap_or(0).to_string();
                let website_redirect_location =
                    output.website_redirect_location().unwrap_or("").to_string();
                let expires_string = output.expires_string().unwrap_or("").to_string();

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
                result.insert(
                    "accept_ranges".to_string(),
                    accept_ranges.as_bytes().to_vec(),
                );

                result.insert(
                    "cache_control".to_string(),
                    cache_control.as_bytes().to_vec(),
                );
                result.insert(
                    "content_disposition".to_string(),
                    content_disposition.as_bytes().to_vec(),
                );
                result.insert(
                    "content_encoding".to_string(),
                    content_encoding.as_bytes().to_vec(),
                );
                result.insert(
                    "content_language".to_string(),
                    content_language.as_bytes().to_vec(),
                );
                result.insert(
                    "content_range".to_string(),
                    content_range.as_bytes().to_vec(),
                );

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

                result.insert("version_id".to_string(), version_id.as_bytes().to_vec());
                result.insert(
                    "delete_marker".to_string(),
                    delete_marker.as_bytes().to_vec(),
                );
                result.insert("expiration".to_string(), expiration.as_bytes().to_vec());
                result.insert("restore".to_string(), restore.as_bytes().to_vec());
                result.insert(
                    "archive_status".to_string(),
                    archive_status.as_bytes().to_vec(),
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
                    "bucket_key_enabled".to_string(),
                    bucket_key_enabled.as_bytes().to_vec(),
                );

                result.insert(
                    "storage_class".to_string(),
                    storage_class.as_bytes().to_vec(),
                );
                result.insert(
                    "replication_status".to_string(),
                    replication_status.as_bytes().to_vec(),
                );
                result.insert(
                    "request_charged".to_string(),
                    request_charged.as_bytes().to_vec(),
                );

                result.insert(
                    "object_lock_mode".to_string(),
                    object_lock_mode.as_bytes().to_vec(),
                );
                result.insert(
                    "object_lock_retain_until_date".to_string(),
                    object_lock_retain_until_date.as_bytes().to_vec(),
                );
                result.insert(
                    "object_lock_legal_hold_status".to_string(),
                    object_lock_legal_hold_status.as_bytes().to_vec(),
                );

                result.insert("missing_meta".to_string(), missing_meta.as_bytes().to_vec());
                result.insert("parts_count".to_string(), parts_count.as_bytes().to_vec());
                result.insert("tag_count".to_string(), tag_count.as_bytes().to_vec());
                result.insert(
                    "website_redirect_location".to_string(),
                    website_redirect_location.as_bytes().to_vec(),
                );

                result.insert("expires".to_string(), expires_string.as_bytes().to_vec());
                result.insert(
                    "expires_string".to_string(),
                    expires_string.as_bytes().to_vec(),
                );
                if let Some(metadata) = output.metadata() {
                    for (key, value) in metadata {
                        let meta_key = format!("metadata_{}", key);
                        result.insert(meta_key, value.as_bytes().to_vec());
                    }
                }

                Ok(result)
            }

            Err(e) => Err(format!("Error: {}", e)),
        }
    })
}
