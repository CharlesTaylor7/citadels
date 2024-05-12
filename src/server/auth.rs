use super::supabase::SignInResponse;
use crate::server::{state::AppState, supabase::EmailCreds};
use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
use std::collections::HashMap;

pub struct Session {
    pub user_id: String,
    pub access_token: String,
    pub refresh_token: String,
    pub expires_in: u64,
}

impl Session {
    pub fn new(json: SignInResponse) -> Self {
        Self {
            user_id: json.user.id,
            access_token: json.access_token,
            refresh_token: json.refresh_token,
            expires_in: json.expires_in,
        }
    }

    pub fn update(&mut self, response: SignInResponse) {
        self.refresh_token = response.refresh_token;
        self.access_token = response.access_token;
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
            cookies = cookies.add(
                Cookie::build(("session_id", (session.user_id.clone())))
                    .max_age(time::Duration::WEEK),
            );
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
    let cookie =
        Cookie::build(("session_id", session.user_id.clone())).max_age(time::Duration::WEEK);
    cookies = cookies.add(cookie);
    state.add_session(session).await;
    Ok(cookies)
}
