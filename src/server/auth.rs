use super::supabase::{RefreshTokenBody, SignInResponse};
use crate::{
    server::{state::AppState, supabase::EmailCreds},
    strings::{AccessToken, RefreshToken, SessionId, UserId},
};
use arcstr::ArcStr;
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
        let session_id = session_id.value();
        self.session_from_id(session_id)
    }

    pub fn session_from_id(&self, session_id: &str) -> Option<Session> {
        self.0.get(session_id).cloned()
    }
}

pub async fn login(
    state: &AppState,
    mut cookies: PrivateCookieJar,
    creds: &EmailCreds<'_>,
) -> anyhow::Result<PrivateCookieJar> {
    match cookies.get("session_id") {
        Some(cookie) => {
            if state
                .sessions
                .read()
                .await
                .session_from_id(cookie.value())
                .is_some()
            {
                anyhow::bail!("Already logged in");
            } else {
                let session = state.supabase.signin_email(creds).await?;
                state.add_session(session).await;
            };
        }
        None => {
            let session = state.supabase.signin_email(creds).await?;
            log::info!("Setting session cookie with 1 week expiry");

            let cookie = Cookie::build(("session_id", session.user.id.to_string()))
                .max_age(time::Duration::WEEK)
                .secure(true)
                .http_only(true);
            cookies = cookies.add(cookie);
            state.add_session(session).await;
        }
    };

    Ok(cookies)
}

pub async fn signup(
    state: &AppState,
    mut cookies: PrivateCookieJar,
    creds: &EmailCreds<'_>,
) -> anyhow::Result<PrivateCookieJar> {
    if cookies.get("session_id").is_some() {
        anyhow::bail!("Already has a session cookie");
    }
    let session = state.supabase.signup_email(creds).await?;

    let cookie = Cookie::build(("session_id", session.user.id.to_string()))
        .max_age(time::Duration::WEEK)
        .secure(true)
        .http_only(true);
    cookies = cookies.add(cookie);
    state.add_session(session).await;
    Ok(cookies)
}
