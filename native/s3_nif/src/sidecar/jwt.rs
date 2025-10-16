use crate::sidecar::get_env_var;
use anyhow::{Error, anyhow};
use chrono::{Duration, Utc};
use jsonwebtoken::{Algorithm, DecodingKey, EncodingKey, Header, Validation, decode, encode};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct DataitemClaims {
    pub bucket_name: String,
    pub load_acc: String,
    pub dataitem_key: String,
    pub exp: i64, // expiration timestamp
    pub iat: i64, // issued-at timestamp
    // x402 data protocol properties
    pub payee: Option<String>,
    pub amount: Option<f64>,
    pub network: Option<String>,
}

pub fn create_signed_dataitem_url(
    bucket_name: &str,
    load_acc: &str,
    dataitem_key: &str,
    expires_in_minutes: i64,
    payee: Option<String>,
    amount: Option<f64>,
    network: Option<String>,
) -> Result<String, Error> {
    let now = Utc::now();
    let exp = now + Duration::minutes(expires_in_minutes);
    let secret_key = get_env_var("PRESIGNED_URL_JWT_PRIV")?;

    let claims = DataitemClaims {
        bucket_name: bucket_name.to_string(),
        load_acc: load_acc.to_string(),
        dataitem_key: dataitem_key.to_string(),
        exp: exp.timestamp(),
        iat: now.timestamp(),
        payee,
        amount,
        network,
    };

    let token = encode(
        &Header::default(),
        &claims,
        &EncodingKey::from_secret(secret_key.as_ref()),
    )?;

    Ok(token)
}

pub(crate) fn validate_dataitem_token(
    token: &str,
    expected_dataitem_key: &str,
) -> Result<DataitemClaims, Error> {
    let secret_key = get_env_var("PRESIGNED_URL_JWT_PRIV")?;
    let token_data = decode::<DataitemClaims>(
        token,
        &DecodingKey::from_secret(secret_key.as_ref()),
        &Validation::new(Algorithm::HS256),
    )?;

    let claims = token_data.claims;

    let claims_key = claims.dataitem_key.trim_end_matches(".ans104");
    let expected_key = expected_dataitem_key.trim_end_matches(".ans104");

    if claims_key != expected_key {
        return Err(anyhow!(
            "Dataitem ID mismatch: expected {}, got {}",
            expected_dataitem_key,
            claims.dataitem_key
        ));
    }

    Ok(claims)
}
