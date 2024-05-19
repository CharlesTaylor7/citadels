use crate::strings::SessionId;
use async_trait::async_trait;
use axum_core::extract::FromRequestParts;
use http::request::Parts;
use tower_cookies::Cookies;

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
