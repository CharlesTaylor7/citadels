use super::auth::{JwtDecoder, Session, Sessions};
use super::supabase::DiscordSigninResponse;
use crate::server::supabase::SupabaseAnonClient;
use crate::server::ws;
use crate::strings::{AccessToken, OAuthCode, OAuthCodeVerifier, RefreshToken, SessionId, UserId};
use crate::{game::Game, lobby::Lobby};
use axum::extract::FromRef;
use axum_extra::extract::cookie::Cookie;
use axum_extra::extract::{cookie, PrivateCookieJar};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tokio::sync::RwLock;

fn new_arc_mutex<T>(item: T) -> Arc<std::sync::Mutex<T>> {
    Arc::new(std::sync::Mutex::new(item))
}

pub struct SessionInfo {
    pub user_id: UserId,
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
    pub expires_in: u64,
}

#[derive(Clone)]
pub struct AppState {
    cookie_signing_key: cookie::Key,
    pub lobby: Arc<std::sync::Mutex<Lobby>>,
    pub game: Arc<std::sync::Mutex<Option<Game>>>,
    pub supabase: SupabaseAnonClient,
    pub sessions: Arc<RwLock<Sessions>>,
    pub connections: Arc<Mutex<ws::Connections>>,
    pub oauth_code_challenges: Arc<RwLock<HashMap<OAuthCode, OAuthCodeVerifier>>>,
}

impl Default for AppState {
    fn default() -> Self {
        let key = std::env::var("COOKIE_SIGNING_KEY").expect("env var COOKIE_SIGNING_KEY not set");

        Self {
            cookie_signing_key: cookie::Key::from(key.as_bytes()),
            lobby: new_arc_mutex(Lobby::default()),
            game: new_arc_mutex(None),
            supabase: SupabaseAnonClient::new(),
            connections: Arc::new(Mutex::new(ws::Connections::default())),
            sessions: Arc::new(RwLock::new(Sessions::default())),
            oauth_code_challenges: Arc::new(RwLock::new(HashMap::default())),
        }
    }
}

impl FromRef<AppState> for cookie::Key {
    fn from_ref(state: &AppState) -> Self {
        state.cookie_signing_key.clone()
    }
}

impl AppState {
    pub async fn session(&self, cookies: &PrivateCookieJar) -> Option<Session> {
        self.sessions.read().await.session_from_cookies(cookies)
    }

    pub async fn session_from_id(&self, session_id: &SessionId) -> Option<Session> {
        self.sessions.read().await.session_from_id(session_id)
    }

    pub async fn logout(&self, cookies: &PrivateCookieJar) -> anyhow::Result<()> {
        let session_id = cookies
            .get("session_id")
            .ok_or(anyhow::anyhow!("not actually logged in"))?;
        let session_id = SessionId::new(session_id.value());
        let mut lock = self.sessions.write().await;
        let session = lock
            .0
            .remove(&session_id)
            .ok_or(anyhow::anyhow!("lost track of session"))?;
        drop(lock);

        self.supabase.logout(&session.access_token).await?;
        self.connections.lock().unwrap().0.remove(&session.user_id);
        Ok(())
    }

    pub async fn add_session(
        &self,
        mut cookies: PrivateCookieJar,
        signin: DiscordSigninResponse,
    ) -> PrivateCookieJar {
        let session_id = SessionId::new(uuid::Uuid::new_v4().to_string());
        let cookie = Cookie::build(("session_id", session_id.to_string()))
            .max_age(time::Duration::WEEK)
            .secure(true)
            .http_only(true);
        cookies = cookies.add(cookie);

        log::info!("{:#?}", signin.access_token.as_str());
        let decoded = JwtDecoder::new().decode(signin.access_token.as_str());
        log::info!("{:#?}", decoded);
        let session = Session {
            username: None,
            session_id,
            user_id: UserId::new("TODO"),
            access_token: signin.access_token,
            refresh_token: signin.refresh_token,
        };
        self.sessions
            .write()
            .await
            .0
            .insert(session.session_id.clone(), session.clone());
        self.clone()
            .spawn_session_refresh_task(session, signin.expires_in);
        cookies
    }

    pub fn spawn_session_refresh_task(self, session: Session, expires_in: u64) {
        let duration = tokio::time::Duration::from_secs(expires_in / 2);
        tokio::task::spawn(async move {
            let mut interval = tokio::time::interval(duration);
            interval.tick().await;
            loop {
                interval.tick().await;
                let token =
                    if let Some(session) = self.sessions.read().await.0.get(&session.session_id) {
                        session.refresh_token.clone()
                    } else {
                        break;
                    };
                match self.supabase.refresh(token).await {
                    Ok(signin) => {
                        if let Some(session) =
                            self.sessions.write().await.0.get_mut(&session.session_id)
                        {
                            session.update(signin);
                        } else {
                            break;
                        }
                    }
                    Err(e) => {
                        log::error!("{}", e);
                        self.sessions.write().await.0.remove(&session.session_id);
                        break;
                    }
                }
            }
            self.connections.lock().unwrap().0.remove(&session.user_id)
        });
    }
}

/* DTOs */
#[derive(Deserialize)]
pub struct OAuthCallbackCode {
    pub code: OAuthCode,
}
#[derive(Deserialize)]
pub struct Signin {
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
    pub expires_in: u64,
}
