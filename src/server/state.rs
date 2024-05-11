use super::auth::Sessions;
use crate::server::supabase::SupabaseAnonClient;
use crate::server::ws;
use crate::{game::Game, lobby::Lobby};
use axum::extract::FromRef;
use axum_extra::extract::{cookie, PrivateCookieJar};
use std::sync::{Arc, Mutex};
use tokio::sync::RwLock;
use tokio::time::Duration;

fn new_arc_mutex<T>(item: T) -> Arc<Mutex<T>> {
    Arc::new(Mutex::new(item))
}

#[derive(Clone)]
pub struct AppState {
    cookie_signing_key: cookie::Key,
    pub lobby: Arc<Mutex<Lobby>>,
    pub game: Arc<Mutex<Option<Game>>>,
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
    pub async fn logout(&self, cookies: &PrivateCookieJar) -> anyhow::Result<()> {
        let session_id = cookies
            .get("session_id")
            .ok_or(anyhow::anyhow!("not actually logged in"))?;
        let session_id = session_id.value();
        let lock = self.sessions.read().await;
        let (index, session) = lock
            .0
            .iter()
            .enumerate()
            .find(|(_, session)| session_id == &session.id)
            .ok_or(anyhow::anyhow!("lost track of session"))?;
        let access_token = session.access_token.clone();
        drop(lock);
        self.sessions.write().await.0.swap_remove(index);
        self.supabase.logout(&access_token).await?;
        Ok(())
    }

    pub async fn refresh_sessions(self, duration: Duration) {
        let mut interval = tokio::time::interval(duration);
        loop {
            interval.tick().await;
            log::info!("Refreshing sessions");
            let mut index = 0;
            loop {
                let sessions_lock = self.sessions.read().await;
                if let Some(session) = sessions_lock.0.get(index) {
                    log::info!("Refreshing {}", session.id);

                    let token = session.refresh_token.clone();
                    let session_id = session.id.clone();
                    drop(sessions_lock);
                    let signin = self.supabase.refresh(&token).await;
                    match signin {
                        Ok(signin) => {
                            let sessions_lock = &mut self.sessions.write().await;
                            if let Some(session) = sessions_lock.0.get_mut(index) {
                                if session.id == session_id {
                                    session.update(signin);
                                } else {
                                    continue;
                                }
                            } else {
                                break;
                            }
                        }
                        Err(e) => {
                            log::error!("{}", e);
                        }
                    }
                    index += 1;
                } else {
                    break;
                }
            }
        }
    }
}
