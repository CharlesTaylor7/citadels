use crate::server::models::SupabaseUser;
use crate::server::state::AppState;
use axum::extract::State;

use axum_core::body::Body;
use futures::Future;
use http::{Request, Response};
use std::pin::Pin;
use std::task::{Context, Poll};
use tower_cookies::Cookies;
use tower_layer::Layer;
use tower_service::Service;

#[derive(Clone)]
pub struct LoggedInService<S> {
    inner: S,
}

impl<S> LoggedInService<S> {
    pub fn new(inner: S) -> Self {
        Self { inner }
    }
}

impl<S> Service<Request<Body>> for LoggedInService<S>
where
    S: Service<Request<Body>, Response = Response<Body>>,
    S::Future: Send,
{
    type Response = Response<Body>;
    //S::Response;
    type Error = S::Error;
    // waiting for TAIT so that we can just do:
    // type Future = impl Future
    // type Future = S::Future;
    type Future =
        Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send + 'static>>;

    #[inline]
    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(&mut self, _req: Request<Body>) -> Self::Future {
        todo!("Reverse engineer what tower-cookies does");
    }
}

fn require_login<ReqBody>(req: &Request<ReqBody>) -> anyhow::Result<SupabaseUser> {
    let _state = req
        .extensions()
        .get::<State<AppState>>()
        .ok_or(anyhow::anyhow!("no app state"))?;
    let cookies = req
        .extensions()
        .get::<Cookies>()
        .ok_or(anyhow::anyhow!("no cookies"))?;
    let _cookie = cookies
        .get("access_token")
        .ok_or(anyhow::anyhow!("no access_token"));

    anyhow::bail!("Hey")
}

#[derive(Clone)]
pub struct LoggedInLayer {
    _private: (),
}

impl LoggedInLayer {
    pub fn new() -> Self {
        Self { _private: () }
    }
}

impl<S> Layer<S> for LoggedInLayer {
    type Service = LoggedInService<S>;

    fn layer(&self, inner: S) -> Self::Service {
        LoggedInService { inner }
    }
}
