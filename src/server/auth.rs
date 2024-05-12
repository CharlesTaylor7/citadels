use super::supabase::SignInResponse;
use crate::server::{state::AppState, supabase::EmailCreds};
use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
use std::collections::HashMap;
use uuid::Uuid;

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
        state.add_session(session).await;
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
      state.add_session(session).await;
      let cookie = Cookie::build(("session_id", session_id)).max_age(time::Duration::WEEK);
      cookies = cookies.add(cookie);
    }
  };

  Ok(cookies)
}
