use super::auth::{Session, Sessions};
use super::supabase::SignInResponse;
use crate::server::supabase::SupabaseAnonClient;
use crate::server::ws;
use crate::strings::SessionId;
use crate::{game::Game, lobby::Lobby};
use axum::extract::FromRef;
use axum_extra::extract::{cookie, PrivateCookieJar};
use std::sync::{Arc, Mutex};
use tokio::sync::RwLock;

fn new_arc_mutex<T>(item: T) -> Arc<std::sync::Mutex<T>> {
    Arc::new(std::sync::Mutex::new(item))
}

#[derive(Clone)]
pub struct AppState {
    cookie_signing_key: cookie::Key,
    pub lobby: Arc<std::sync::Mutex<Lobby>>,
    pub game: Arc<std::sync::Mutex<Option<Game>>>,
    pub connections: Arc<Mutex<ws::Connections>>,
    pub supabase: SupabaseAnonClient,
    pub sessions: Arc<RwLock<Sessions>>,
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

    pub async fn add_session(&self, session_id: SessionId, signin: SignInResponse) {
        let session = Session {
            session_id,
            user_id: signin.user.id,
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
