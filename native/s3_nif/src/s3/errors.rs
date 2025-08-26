use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};

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
