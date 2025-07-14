use crate::config::S3Config;
use crate::s3::{create_bucket, get_object_with_metadata, push_object};
use aws_sdk_s3::Client;
use aws_sdk_s3::primitives::ByteStream;
use axum::{
    Router,
    extract::{Path, State},
    http::{HeaderMap, HeaderValue, StatusCode},
    response::{IntoResponse, Response},
    routing::{get, put},
};
use bytes::Bytes;
use std::sync::Arc;
use tower_http::cors::CorsLayer;

#[derive(Clone)]
pub struct S3Server {
    client: Arc<Client>,
    config: S3Config,
}

impl S3Server {
    pub fn new(client: Client, config: S3Config) -> Self {
        Self {
            client: Arc::new(client),
            config,
        }
    }

    pub async fn from_config(config: S3Config) -> Result<Self, Box<dyn std::error::Error>> {
        let client = config.build_client().await?;
        Ok(Self::new(client, config.clone()))
    }

    pub fn app(&self) -> Router {
        Router::new()
            .route("/:bucket", put(create_bucket_handler))
            .route("/:bucket/:key", put(put_object_handler))
            .route("/:bucket/:key", get(get_object_handler))
            .layer(CorsLayer::permissive())
            .with_state(self.clone())
    }

    pub async fn serve(self) -> Result<(), Box<dyn std::error::Error>> {
        let app = self.app();
        let listener =
            tokio::net::TcpListener::bind(format!("0.0.0.0:{}", self.config.port)).await?;
        println!(
            "S3 compatible server listening on port {}",
            self.config.port
        );
        println!("Using endpoint: {}", self.config.endpoint);
        println!("Using region: {}", self.config.region);
        println!("Using path-style access: {}", self.config.force_path_style);
        axum::serve(listener, app).await?;
        Ok(())
    }
}

async fn create_bucket_handler(
    State(server): State<S3Server>,
    Path(bucket): Path<String>,
) -> Result<StatusCode, S3Error> {
    create_bucket(&server.client, &bucket).await?;
    Ok(StatusCode::OK)
}

async fn put_object_handler(
    State(server): State<S3Server>,
    Path((bucket, key)): Path<(String, String)>,
    body: Bytes,
) -> Result<Response, S3Error> {
    let byte_stream = ByteStream::from(body);
    let response = push_object(&server.client, &bucket, &key, byte_stream).await?;

    let headers = build_headers_with_etag(response.e_tag())?;
    Ok((StatusCode::OK, headers).into_response())
}

async fn get_object_handler(
    State(server): State<S3Server>,
    Path((bucket, key)): Path<(String, String)>,
) -> Result<Response, S3Error> {
    let (bytes, etag) = get_object_with_metadata(&server.client, &bucket, &key).await?;
    let headers = build_headers_with_etag(etag.as_deref())?;
    Ok((StatusCode::OK, headers, bytes).into_response())
}

fn build_headers_with_etag(etag: Option<&str>) -> Result<HeaderMap, S3Error> {
    let mut headers = HeaderMap::new();
    if let Some(etag) = etag {
        headers.insert(
            "ETag",
            HeaderValue::from_str(etag).map_err(|e| S3Error::Internal(e.to_string()))?,
        );
    }
    Ok(headers)
}

#[derive(Debug)]
pub enum S3Error {
    AwsError(aws_sdk_s3::Error),
    Internal(String),
}

impl From<aws_sdk_s3::Error> for S3Error {
    fn from(error: aws_sdk_s3::Error) -> Self {
        S3Error::AwsError(error)
    }
}

impl IntoResponse for S3Error {
    fn into_response(self) -> Response {
        match self {
            S3Error::AwsError(e) => {
                (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()).into_response()
            }
            S3Error::Internal(e) => (StatusCode::INTERNAL_SERVER_ERROR, e).into_response(),
        }
    }
}
