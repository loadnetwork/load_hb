use aws_config::{BehaviorVersion, Region};
use aws_sdk_s3::Client;

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
