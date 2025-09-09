use axum::{
    body::Body,
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
    response::Response,
    routing::get,
    Router,
};
use futures::TryStreamExt;
use tokio_util::io::ReaderStream;
use tower_http::cors::CorsLayer;
use anyhow::Error;

use crate::sidecar::{AppState, ans104, range, get_env_var};
use crate::s3::{create_s3_client, get_object, DATAITEMS_BUCKET, DATAITEMS_DIR};


#[derive(serde::Deserialize)]
struct RangeQuery {
    range: Option<String>,
}

#[derive(Debug)]
struct SidecarConfig {
    endpoint: String,
    access_key_id: String,
    secret_access_key: String,
    region: String,
    port: String
}

impl SidecarConfig {
    pub fn load_env() -> Result<Self, Error> {
        Ok(Self { endpoint: get_env_var("ENDPOINT")?, access_key_id: get_env_var("ACCESS_KEY_ID")?, secret_access_key: get_env_var("SECRET_ACCESS_KEY")?, region: get_env_var("REGION")?, port: get_env_var("PORT")? })
    }
}

pub async fn serve() -> Result<(), Box<dyn std::error::Error>> {

    let sidecar_config = SidecarConfig::load_env()?;

    println!("sidecar_config {:?}", sidecar_config);

    println!("offchain ANS-104 streaming sidecar v{}", env!("CARGO_PKG_VERSION"));
    println!("running on port {}", sidecar_config.port);

    let s3_client = create_s3_client(&sidecar_config.endpoint, &sidecar_config.access_key_id, &sidecar_config.secret_access_key, &sidecar_config.region, Some(true)).await;
    let app_state = AppState { s3_client };

    let app = Router::new()
        .route("/resolve/:dataitem_id", get(resolve_dataitem))
        .route("/health", get(|| async { "sidecar running" }))
        .layer(CorsLayer::permissive())
        .with_state(app_state);

    let listener = tokio::net::TcpListener::bind(format!("127.0.0.1:{}", sidecar_config.port)).await?;

    axum::serve(listener, app).await?;
    Ok(())
}

async fn resolve_dataitem(
    Path(dataitem_id): Path<String>,
    Query(params): Query<RangeQuery>,
    headers: HeaderMap,
    State(state): State<AppState>,
) -> Result<Response, StatusCode> {
    println!("Resolving dataitem: {}", dataitem_id);

    let range_str = params.range
        .or_else(|| headers.get("range").and_then(|h| h.to_str().ok().map(String::from)))
        .unwrap_or_default();

    let key = format!("{}/{}.ans104", DATAITEMS_DIR, dataitem_id);

    // Fetch header
    let header_obj = get_object(&state.s3_client, DATAITEMS_BUCKET, &key, "bytes=0-2047")
        .await.map_err(|_| StatusCode::NOT_FOUND)?;
    let header_bytes = header_obj.body.collect().await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
        .into_bytes()
        .to_vec();

    let (mime_type, data_offset) = ans104::parse_ans104_header(&header_bytes)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    if range_str.is_empty() {
        stream_data_section(&state.s3_client, &key, data_offset, None, &mime_type).await
    } else {
        let (start, end_opt) = range::parse_range(&range_str).ok_or(StatusCode::RANGE_NOT_SATISFIABLE)?;
        stream_data_section(&state.s3_client, &key, data_offset, Some((start, end_opt)), &mime_type).await
    }
}

async fn stream_data_section(
    client: &aws_sdk_s3::Client,
    key: &str,
    data_offset: usize,
    range: Option<(u64, Option<u64>)>,
    mime_type: &str,
) -> Result<Response, StatusCode> {
    let s3_range = match range {
        Some((start, end_opt)) => {
            let phys_start = data_offset as u64 + start;
            match end_opt {
                Some(end) => format!("bytes={}-{}", phys_start, data_offset as u64 + end),
                None => format!("bytes={}-", phys_start),
            }
        }
        None => format!("bytes={}-", data_offset),
    };

    let obj = get_object(client, DATAITEMS_BUCKET, key, &s3_range)
        .await.map_err(|_| StatusCode::NOT_FOUND)?;

    let content_length = obj.content_length().unwrap_or(0) as u64;
    let reader = obj.body.into_async_read();
    let stream = ReaderStream::new(reader)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e));
    let body = Body::from_stream(stream);

    let mut response = Response::builder()
        .header("content-type", mime_type)
        .header("content-length", content_length.to_string())
        .header("accept-ranges", "bytes")
        .header("access-control-allow-origin", "*")
        .header("access-control-allow-headers", "*")
        .header("access-control-allow-methods", "GET, OPTIONS");

    if range.is_some() {
        response = response.status(StatusCode::PARTIAL_CONTENT);
        if let Some((start, end_opt)) = range {
            let end = end_opt.unwrap_or(start + content_length - 1);
            response = response.header(
                "content-range",
                format!("bytes {}-{}/{}", start, end, content_length)
            );
        }
    }

    response.body(body).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}
