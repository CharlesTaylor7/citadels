use poem::{
    http::StatusCode, Body, Endpoint, IntoResponse, Request, RequestParts, Response, Result,
};
use std::sync::Arc;

struct HtmxOrSpa<E1, E2> {
    primary: Arc<E1>,
    fallback: Arc<E2>,
}

impl<E1, E2> HtmxOrSpa<E1, E2> {
    fn new(primary: E1, fallback: E2) -> Self {
        Self {
            primary: Arc::new(primary),
            fallback: Arc::new(fallback),
        }
    }
}

impl<E1, E2> Endpoint for HtmxOrSpa<E1, E2>
where
    E1: Endpoint + Send + Sync + 'static,
    E2: Endpoint + Send + Sync + 'static,
{
    type Output = Response;
    async fn call(&self, mut req: Request) -> Result<Response> {
        let body = req.take_body();
        let bytes = body.into_vec().await.unwrap();
        //let (parts, _) = req.into_parts();
        let hyper_req: hyper::Request<_> = req.into();
        let (parts, _) = hyper_req.into_parts();

        // let request: poem::Request = Request::from_parts(parts.into(), bytes.into());
        // let mapped = hyper_req.map(|b| Body::from(b));
        // let cloned = mapped.clone();
        // Ok(StatusCode::OK.into_response())
        todo!()
    }
}
