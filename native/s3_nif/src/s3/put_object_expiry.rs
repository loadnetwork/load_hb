use aws_sdk_s3::Client;
use aws_sdk_s3::operation::put_object::PutObjectOutput;
use aws_sdk_s3::primitives::ByteStream;
use aws_sdk_s3::types::{
    BucketLifecycleConfiguration, ExpirationStatus, LifecycleExpiration, LifecycleRule,
    LifecycleRuleFilter, Tag,
};
use aws_smithy_types::error::operation::BuildError;

use crate::s3::{MAX_OBJECT_EXPIRY_DAYS, MIN_OBJECT_EXPIRY_DAYS};

pub async fn put_object_expiry(
    client: &Client,
    bucket_name: &str,
    key: &str,
    body: ByteStream,
    expiry_days: i32,
) -> Result<PutObjectOutput, aws_sdk_s3::Error> {
    let _ = add_expiry_rule_if_needed(client, bucket_name, expiry_days).await?;

    let response = client
        .put_object()
        .bucket(bucket_name)
        .key(key)
        .tagging(format!("expiry-days={}", expiry_days))
        .body(body)
        .send()
        .await?;

    Ok(response)
}

async fn add_expiry_rule_if_needed(
    client: &Client,
    bucket: &str,
    expiry_days: i32,
) -> Result<(), aws_sdk_s3::Error> {
    if expiry_days < MIN_OBJECT_EXPIRY_DAYS || expiry_days > MAX_OBJECT_EXPIRY_DAYS {
        let build_error = BuildError::other(format!(
            "expiry days must be between {} and {}",
            MIN_OBJECT_EXPIRY_DAYS, MAX_OBJECT_EXPIRY_DAYS
        ));
        return Err(aws_sdk_s3::Error::from(build_error));
    }
    let rule_id = format!("expire-{}-days", expiry_days);

    let existing_config = client
        .get_bucket_lifecycle_configuration()
        .bucket(bucket)
        .send()
        .await
        .ok();

    let mut rules = if let Some(config) = existing_config {
        config.rules().to_vec()
    } else {
        Vec::new()
    };

    // update lifecycle config if the rule does not exist already
    if !rules.iter().any(|r| r.id() == Some(rule_id.as_str())) {
        let tag = Tag::builder()
            .key("expiry-days")
            .value(expiry_days.to_string())
            .build()?;

        let new_rule = LifecycleRule::builder()
            .id(&rule_id)
            .status(ExpirationStatus::Enabled)
            .filter(LifecycleRuleFilter::builder().tag(tag).build())
            .expiration(LifecycleExpiration::builder().days(expiry_days).build())
            .build();

        rules.push(new_rule?);

        client
            .put_bucket_lifecycle_configuration()
            .bucket(bucket)
            .lifecycle_configuration(
                BucketLifecycleConfiguration::builder()
                    .set_rules(Some(rules))
                    .build()?,
            )
            .send()
            .await?;

        println!(
            "created new lifecycle rule for {} days for bucket {}",
            expiry_days, bucket
        );
    }

    Ok(())
}
