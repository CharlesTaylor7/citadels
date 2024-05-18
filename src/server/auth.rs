use super::supabase::SignInResponse;
use crate::{
    server::{state::AppState, supabase::EmailCreds},
    strings::{AccessToken, RefreshToken, SessionId, UserId},
};
use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
use std::collections::HashMap;

#[derive(Clone)]
pub struct Session {
    pub session_id: SessionId,
    pub user_id: UserId,
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
}

impl Session {
    pub fn update(&mut self, response: SignInResponse) {
        self.access_token = response.access_token;
        self.refresh_token = response.refresh_token;
    }
}

#[derive(Default)]
pub struct Sessions(pub HashMap<SessionId, Session>);

impl Sessions {
    pub fn session_from_cookies(&self, cookies: &PrivateCookieJar) -> Option<Session> {
        let session_id = cookies.get("session_id")?;
        let session_id = SessionId::new(session_id.value());
        self.0.get(&session_id).cloned()
    }

    pub fn session_from_id(&self, session_id: &SessionId) -> Option<Session> {
        self.0.get(session_id).cloned()
    }
}
