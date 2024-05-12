use super::auth::Sessions;
use crate::server::supabase::SupabaseAnonClient;
use crate::server::ws;
use crate::{game::Game, lobby::Lobby};
use axum::extract::FromRef;
use axum_extra::extract::{cookie, PrivateCookieJar};
use std::sync::{Arc, Mutex};
use tokio::sync::RwLock;
use tokio::time::Duration;

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
    pub async fn logout(&self, cookies: &PrivateCookieJar) -> anyhow::Result<()> {
        let session_id = cookies
            .get("session_id")
            .ok_or(anyhow::anyhow!("not actually logged in"))?;
        let session_id = session_id.value();
        let mut lock = self.sessions.write().await;
        let session = lock
            .0
            .remove(session_id)
            .ok_or(anyhow::anyhow!("lost track of session"))?;
        drop(lock);

        self.supabase.logout(&session.access_token).await?;
        //async { self.connections.lock().await.0.remove(session_id) },
        Ok(())
    }

    pub async fn refresh_sessions(self, duration: Duration) {
        let mut interval = tokio::time::interval(duration);
        loop {
            interval.tick().await;
            log::info!("Refreshing sessions");
            let ids: Vec<String> = self.sessions.read().await.0.keys().cloned().collect();
            for id in ids {
                let lock = self.sessions.read().await;
                if let Some(session) = lock.0.get(&id) {
                    log::info!("Refreshing {}", session.user_id);
                    let token = session.refresh_token.clone();
                    drop(lock);
                    let signin = self.supabase.refresh(&token).await;
                    match signin {
                        Ok(signin) => {
                            if let Some(session) =
                                self.sessions.write().await.0.get_mut(&signin.user.id)
                            {
                                session.update(signin);
                            }
                        }
                        Err(e) => {
                            log::error!("{}", e);
                        }
                    }
                }
            }
        }
    }
}
