use super::errors::AppError;
use axum::response::{IntoResponse, Response};

pub type AppResponse = Result<Response, AppError>;
pub fn ok(success: impl IntoResponse) -> AppResponse {
    Ok(success.into_response())
}

pub fn err(failure: impl Into<AppError>) -> AppResponse {
    Err(failure.into())
}
