use std::collections::HashMap;

use super::supabase::SignInResponse;
use crate::server::{state::AppState, supabase::EmailCreds};
use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
use uuid::Uuid;

pub struct Session {
    pub user_id: String,
    pub access_token: String,
    pub refresh_token: String,
    pub expires_at: usize, // utc epoch in seconds
}

impl Session {
    pub fn update(&mut self, response: SignInResponse) {
        self.refresh_token = response.refresh_token;
        self.access_token = response.access_token;
        self.expires_at = response.expires_at;
    }
}

#[derive(Default)]
pub struct Sessions(pub HashMap<String, Session>);

impl Sessions {
    pub fn session_from_cookies(&self, cookies: &PrivateCookieJar) -> Option<&Session> {
        let session_id = cookies.get("session_id")?;
        let session_id = session_id.value();
        self.session_from_id(session_id)
    }

    pub fn session_from_id(&self, session_id: &str) -> Option<&Session> {
        self.0.get(session_id)
    }
}

pub async fn signin_or_signup(
    state: &AppState,
    mut cookies: PrivateCookieJar,
    creds: &EmailCreds<'_>,
) -> anyhow::Result<PrivateCookieJar> {
    match cookies.get("session_id") {
        Some(cookie) => {
            let session_id = cookie.value();
            let sessions = state.sessions.read().await;
            if sessions.session_from_id(session_id).is_none() {
                drop(sessions);
                let session = state.supabase.signin_email(creds).await?;
                state
                    .sessions
                    .write()
                    .await
                    .0
                    .insert(session.user_id.clone(), session);
            };
        }
        None => {
            log::info!("Setting new player_id cookie with 1 week expiry");
            let session_id = Uuid::new_v4().to_string();

            let supabase = &state.supabase;
            let signin = supabase.signin_email(creds).await;

            let session = match signin {
                Ok(session) => session,
                Err(e) => {
                    log::error!("{}", e);
                    supabase.signup_email(creds).await?
                }
            };
            state
                .sessions
                .write()
                .await
                .0
                .insert(session.user_id.clone(), session);
            let cookie = Cookie::build(("session_id", session_id)).max_age(time::Duration::WEEK);
            cookies = cookies.add(cookie);
        }
    };

    Ok(cookies)
}
