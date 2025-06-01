use arcstr::ArcStr;
use poem::error::ResponseError;
use poem::http::StatusCode;
use poem_openapi::ApiResponse;
use std::fmt::Display;
use thiserror::Error;

pub type RequestResult<T> = std::result::Result<T, RequestError>;

#[derive(Error, Debug)]
pub enum RequestError {
    #[error("Database error")]
    Database(#[from] sqlx::Error),

    #[error("{0:#?}")]
    FailedAction(#[from] color_eyre::Report),
}

impl ResponseError for RequestError {
    fn status(&self) -> StatusCode {
        StatusCode::INTERNAL_SERVER_ERROR
    }
}
