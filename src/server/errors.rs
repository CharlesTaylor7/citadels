use anyhow::anyhow;
use axum::response::{IntoResponse, Response};
use http::StatusCode;
use std::borrow::Cow;
use std::error::Error;

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        match self {
            AppError::Internal { error } => {
                log::error!("\n{}\n{}", error, error.backtrace());
                (StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error").into_response()
            }

            AppError::Database { error } => {
                log::error!("\n{}", error);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("Sqlx Error\n{}\n{:#?}", error, error.source()),
                )
                    .into_response()
            }

            AppError::Serialization { error } => {
                log::error!("\n{}", error);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!(
                        "Serde Error\n{}\n{:#?}",
                        error,
                        error.is_data(),
                        //error.source()
                    ),
                )
                    .into_response()
            }

            AppError::Reqwest { error } => {
                log::error!("\n{}", error);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!(
                        "Reqwest Error\n{}\n{:#?}\nStatus Code{:#?}",
                        error,
                        error.source(),
                        error.status(),
                    ),
                )
                    .into_response()
            }

            AppError::FormFeedback { message } => {
                log::info!("Form feedback: {message:#?}");
                (
                    StatusCode::BAD_REQUEST,
                    [("HX-Retarget", "#error"), ("HX-Reswap", "innerHTML")],
                    message,
                )
                    .into_response()
            }
        }
    }
}

#[derive(Debug)]
pub enum AppError {
    Internal { error: anyhow::Error },
    Database { error: sqlx::Error },
    Serialization { error: serde_json::Error },
    Reqwest { error: reqwest::Error },
    FormFeedback { message: String },
}

impl From<Cow<'static, str>> for AppError {
    fn from(error: Cow<'static, str>) -> Self {
        AppError::Internal {
            error: anyhow!("{}", error),
        }
    }
}

impl From<anyhow::Error> for AppError {
    fn from(error: anyhow::Error) -> Self {
        AppError::Internal { error }
    }
}

impl From<sqlx::Error> for AppError {
    fn from(error: sqlx::Error) -> Self {
        AppError::Database { error }
    }
}

impl From<serde_json::Error> for AppError {
    fn from(error: serde_json::Error) -> Self {
        AppError::Serialization { error }
    }
}

impl From<reqwest::Error> for AppError {
    fn from(error: reqwest::Error) -> Self {
        AppError::Reqwest { error }
    }
}

// TODO: Axe this
pub type AnyhowResponse = Result<Response, AnyhowError>;

// Make our own error that wraps `anyhow::Error`.
pub struct AnyhowError(pub anyhow::Error);

// Tell axum how to convert `AppError` into a response.
impl IntoResponse for AnyhowError {
    fn into_response(self) -> Response {
        log::error!("\n{}\n{}", self.0, self.0.backtrace());
        (StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error").into_response()
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
