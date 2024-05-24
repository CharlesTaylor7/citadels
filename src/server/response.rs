use super::errors::AppError;
use axum::response::Response;

pub type AppResponse = Result<Response, AppError>;
pub fn ok(success: impl Into<Response>) -> AppResponse {
    Ok(success.into())
}

pub fn err(failure: impl Into<AppError>) -> AppResponse {
    Err(failure.into())
}
