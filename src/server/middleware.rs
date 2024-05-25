use std::pin::Pin;
use axum::response::{IntoResponse, Redirect};
use futures::Future;
use axum_core::body::Body;
use http::{Request, Response};
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

impl<ReqBody, S> Service<Request<ReqBody>> for LoggedInService<S>
where
    S: Service<Request<ReqBody>, Response = Response<Body>>,
{
    type Response = Response<Body>;
    //S::Response;
    type Error = S::Error;
    // waiting for TAIT so that we can just do:
    // type Future = impl Future
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>>>>; 


    #[inline]
    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(&mut self, req: Request<ReqBody>) -> Self::Future {
        let redirect = req.extensions().get::<Cookies>().and_then(|cookies| cookies.get("access_token")).is_none();
        Box::pin(async {
            if redirect {
                Ok(Redirect::to("/login").into_response())
            } else {
                self.inner.call(req).await
            }
        })
    }
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
