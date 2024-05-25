use crate::server::auth;
use futures::Future;
use http::{Request, Response};
use rename_future::rename_future;
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

impl<ReqBody, ResBody, S> Service<Request<ReqBody>> for LoggedInService<S>
where
    S: Service<Request<ReqBody>, Response = Response<ResBody>>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = LoginServiceFuture;

    #[inline]
    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(&mut self, req: Request<ReqBody>) -> Self::Future {
        if let Some(cookies) = req.extensions().get::<Cookies>() {
            log::info!("TODO: LoggedIn middleware")
        }

        //self.inner.call(req)
        require_logged_in()
    }
}

#[rename_future(LoginServiceFuture)]
async fn require_logged_in() -> Result<ResponseBody {
    return 3;
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
