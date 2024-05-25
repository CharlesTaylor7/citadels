use axum::response::{IntoResponse, Response};
use http::StatusCode;
use std::borrow::Cow;
use thiserror::Error;

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        match self {
            AppError::Internal { error } => (
                StatusCode::INTERNAL_SERVER_ERROR,
                if cfg!(feature = "dev") {
                    Cow::Owned(format!(
                        "Internal Server Error\n{}\n{}",
                        error,
                        error.backtrace()
                    ))
                } else {
                    Cow::Borrowed("Internal Server Error")
                },
            )
                .into_response(),

            AppError::FormFeedback { message } => (
                StatusCode::BAD_REQUEST,
                [("HX-Retarget", "#error"), ("HX-Reswap", "innerHTML")],
                message,
            )
                .into_response(),
        }
    }
}

#[derive(Debug)]
pub enum AppError {
    Internal { error: anyhow::Error },
    FormFeedback { message: String },
}

impl From<anyhow::Error> for AppError {
    fn from(error: anyhow::Error) -> Self {
        AppError::Internal { error }
    }
}

// TODO: Axe this
pub type AnyhowResponse = Result<Response, AnyhowError>;

// Make our own error that wraps `anyhow::Error`.
pub struct AnyhowError(pub anyhow::Error);

// Tell axum how to convert `AppError` into a response.
impl IntoResponse for AnyhowError {
    fn into_response(self) -> Response {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            if cfg!(feature = "dev") {
                Cow::Owned(format!(
                    "Internal Server Error\n{}\n{}",
                    self.0,
                    self.0.backtrace()
                ))
            } else {
                Cow::Borrowed("Internal Server Error")
            },
        )
            .into_response()
    }
}

// This enables using `?` on functions that return `Result<_, anyhow::Error>` to turn them into
// `Result<_, AppError>`. That way you don't need to do that manually.
impl<E> From<E> for AnyhowError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        Self(err.into())
    }
}
