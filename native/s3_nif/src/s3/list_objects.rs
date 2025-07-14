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
