use poem::{
    Endpoint, EndpointExt, IntoResponse, Middleware, Request, Response, Result, Route, Server, get,
    handler, listener::TcpListener, session::Session,
};

pub struct PlayerSessions;

impl<E: Endpoint> Middleware<E> for PlayerSessions {
    type Output = PlayerSessionsImpl<E>;

    fn transform(&self, endpoint: E) -> Self::Output {
        PlayerSessionsImpl(endpoint)
    }
}

pub struct PlayerSessionsImpl<E>(E);

impl<E: Endpoint> Endpoint for PlayerSessionsImpl<E> {
    type Output = E::Output;

    async fn call(&self, mut req: Request) -> Result<Self::Output> {
        let exts = req.extensions_mut();
        let session = exts.get::<Session>();
        session.get_or_insert_with(||
        )

        self.0.call(req).await
    }
}
