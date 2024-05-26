use std::{cell::RefCell, marker::PhantomData};

use async_trait::async_trait;
use axum::extract::FromRequestParts;
use biscotti::{Processor, ProcessorConfig, RequestCookies};
use http::StatusCode;

thread_local! {
    pub static HEADER: RefCell<Option<&'static mut str>> = RefCell::new(None);
}

#[allow(dead_code)]
struct Cookies<S> {
    inner: RequestCookies<'static>,
    _priv: PhantomData<S>,
}

#[allow(unused)]
#[async_trait]
impl<S> FromRequestParts<S> for Cookies<S> {
    type Rejection = StatusCode;
    async fn from_request_parts<'a>(
        parts: &'a mut http::request::Parts,
        _state: &S,
    ) -> Result<Self, Self::Rejection> {
        let cookies: RequestCookies<'static> = if let Some(header) = parts.headers.get("cookie") {
            let parsed = header.to_str().ok().and_then(|header| {
                let config: Processor = ProcessorConfig::default().into();
                todo!("Biscotti doesn't play well with axum due to lifetimes")
                //RequestCookies::parse_header(&cloned, &config).ok()
            });
            match parsed {
                Some(cookies) => cookies,
                None => return Err(StatusCode::BAD_REQUEST),
            }
        } else {
            RequestCookies::new()
        };
        Ok(Cookies {
            inner: cookies,
            _priv: PhantomData,
        })
    }
}
