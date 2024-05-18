use crate::strings::SessionId;
use async_trait::async_trait;
use axum_core::extract::FromRequestParts;
use http::{request::Parts, StatusCode};
use http::{Request, Response};
use std::task::{Context, Poll};
use tower_cookies::{Cookie, Cookies};
use tower_layer::Layer;
use tower_service::Service;

#[async_trait]
impl<S> FromRequestParts<S> for SessionId
where
    S: Sync + Send,
{
    type Rejection = ();

    async fn from_request_parts(parts: &mut Parts, _state: &S) -> Result<Self, Self::Rejection> {
        get_session_id(parts).ok_or(())
    }
}

fn get_session_id(parts: &mut Parts) -> Option<SessionId> {
    let cookies = parts.extensions.get::<Cookies>()?;
    let cookie = cookies.get("session_id")?;
    Some(SessionId::new(cookie.value()))
}

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
            log::info!("{:#?}", cookies);
            if cookies.get("session_id").is_none() {
                let session_id = uuid::Uuid::new_v4().simple().to_string();
                let cookie = Cookie::build(("session_id", session_id.to_string()))
                    .max_age(time::Duration::WEEK)
                    .http_only(true);

                #[cfg(not(feature = "dev"))]
                let cookie = cookie.secure(true);
                log::info!("Setting new session cookie:\n{}", session_id);

                cookies.add(cookie.into());
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
