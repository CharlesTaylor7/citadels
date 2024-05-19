use http::{Request, Response};
use std::task::{Context, Poll};
use tower_cookies::{Cookies};
use tower_layer::Layer;
use tower_service::Service;

use crate::server::auth;

#[derive(Clone)]
pub struct SessionCookie<S> {
    inner: S,
}

impl<S> SessionCookie<S> {
    pub fn new(inner: S) -> Self {
        Self { inner }
    }
}

impl<ReqBody, ResBody, S> Service<Request<ReqBody>> for SessionCookie<S>
where
    S: Service<Request<ReqBody>, Response = Response<ResBody>>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = S::Future;

    #[inline]
    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(&mut self, req: Request<ReqBody>) -> Self::Future {
        if let Some(cookies) = req.extensions().get::<Cookies>() {
            if cookies.get("session_id").is_none() {
                let session_id = uuid::Uuid::new_v4().simple().to_string();
                log::info!("Setting new session cookie:\n{}", session_id);
                cookies.add(auth::cookie("session_id", session_id, time::Duration::WEEK));
            }
        }

        self.inner.call(req)
    }
}

#[derive(Clone)]
pub struct SessionCookieLayer {
    _private: (),
}

impl SessionCookieLayer {
    pub fn new() -> Self {
        Self { _private: () }
    }
}

impl<S> Layer<S> for SessionCookieLayer {
    type Service = SessionCookie<S>;

    fn layer(&self, inner: S) -> Self::Service {
        SessionCookie { inner }
    }
}
