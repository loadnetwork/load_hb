use aws_sdk_s3::{Client, operation::list_objects::ListObjectsOutput};


pub async fn list_objects(
    client: &Client,
    bucket: &str,
    prefix: &str,
    delimiter: &str,
    marker: &str,
    max_keys: &str,
) -> Result<ListObjectsOutput, aws_sdk_s3::Error> {
    let mut request = client
        .list_objects()
        .bucket(bucket);

    // only add params if they are not empty strings so we dont use Option and complicate the erlang nif side
    if !prefix.is_empty() {
        request = request.prefix(prefix);
    }

    if !delimiter.is_empty() {
        request = request.delimiter(delimiter);
    }

    if !marker.is_empty() {
        request = request.marker(marker);
    }

    if !max_keys.is_empty() {
        if let Ok(max_keys_int) = max_keys.parse::<i32>() {
            request = request.max_keys(max_keys_int);
        }
    }

    let result = request.send().await?;
    Ok(result)
}

// Greenfield requirement: https://github.com/bnb-chain/greenfield-storage-provider/blob/master/docs/modules/piece-store.md#compatible-with-multi-object-storage
pub async fn list_all_objects(
    client: &Client,
    bucket: &str,
    prefix: &str,
) -> Result<Vec<aws_sdk_s3::types::Object>, aws_sdk_s3::Error> {
    let mut all_objects = Vec::new();
    let mut marker = None;
    
    loop {
        let mut request = client.list_objects().bucket(bucket);
        
        if !prefix.is_empty() {
            request = request.prefix(prefix);
        }
        
        if let Some(marker_val) = &marker {
            request = request.marker(marker_val);
        }
        
        let output = request.send().await?;
        
        let contents = output.contents();
        if !contents.is_empty() {
            all_objects.extend(contents.iter().cloned());
        }
        
        // check if there are more pages
        if output.is_truncated() == Some(true) {
            marker = output.next_marker()
                .map(|s| s.to_string())
                .or_else(|| {
                    contents.last()
                        .and_then(|obj| obj.key())
                        .map(|s| s.to_string())
                });
        } else {
            break; // no more pages
        }
    }
    
    Ok(all_objects)
}
