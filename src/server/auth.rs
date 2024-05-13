use super::supabase::SignInResponse;
use crate::server::{state::AppState, supabase::EmailCreds};
use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
use std::borrow::Cow;
use std::{collections::HashMap, sync::Arc};

#[derive(Clone)]
pub struct Session {
    pub access_token: Arc<str>,
    pub refresh_token: Arc<str>,
}

impl Session {
    pub fn update(&mut self, response: SignInResponse) {
        self.refresh_token = response.refresh_token;
        self.access_token = response.access_token;
    }
}

#[derive(Default)]
pub struct Sessions(pub HashMap<Arc<str>, Session>);

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

            let user_id = session.user.id.as_ref().to_owned();
            let mut cookie = Cookie::new("session_id", user_id);
            cookie.set_max_age(time::Duration::WEEK);
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
    let cookie = Cookie::build(("session_id", String::from(session.user.id.as_ref())))
        .max_age(time::Duration::WEEK);
    cookies = cookies.add(cookie);
    state.add_session(session).await;
    Ok(cookies)
}
