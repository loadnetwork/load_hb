use jsonwebtoken::{encode, decode, Header, Algorithm, Validation, EncodingKey, DecodingKey};
use serde::{Deserialize, Serialize};
use chrono::{Utc, Duration};
use anyhow::{anyhow, Error};
use crate::sidecar::get_env_var;

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct DataitemClaims {
    pub bucket_name: String,
    pub load_acc: String,
    pub dataitem_id: String,
    pub exp: i64, // expiration timestamp
    pub iat: i64, // issued-at timestamp
}

pub fn create_signed_dataitem_url(
    bucket_name: &str,
    load_acc: &str,
    dataitem_id: &str,
    expires_in_minutes: i64,
) -> Result<String, Error> {
    let now = Utc::now();
    let exp = now + Duration::minutes(expires_in_minutes);
    let secret_key = get_env_var("PRESIGNED_URL_JWT_PRIV")?;
    
    let claims = DataitemClaims {
        bucket_name: bucket_name.to_string(),
        load_acc: load_acc.to_string(),
        dataitem_id: dataitem_id.to_string(),
        exp: exp.timestamp(),
        iat: now.timestamp(),
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
    expected_dataitem_id: &str,
) -> Result<DataitemClaims, Error> {
    let secret_key = get_env_var("PRESIGNED_URL_JWT_PRIV")?;
    let token_data = decode::<DataitemClaims>(
        token,
        &DecodingKey::from_secret(secret_key.as_ref()),
        &Validation::new(Algorithm::HS256),
    )?;
    
    let claims = token_data.claims;
    
    if claims.dataitem_id != expected_dataitem_id {
        return Err(anyhow!("Dataitem ID mismatch"));
    }
    
    // let now = Utc::now().timestamp();
    // if claims.exp < now {
    //     return Err(anyhow!("Token expired"));
    // }
    
    Ok(claims)
}
