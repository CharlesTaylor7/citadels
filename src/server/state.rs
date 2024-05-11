use crate::server::supabase::SupabaseAnonClient;
use crate::server::ws;
use crate::{game::Game, lobby::Lobby};
use axum::extract::FromRef;
use axum_extra::extract::cookie;
use std::sync::{Arc, Mutex, RwLock};

#[derive(Clone)]
pub struct AppState {
    cookie_signing_key: cookie::Key,
    pub lobby: Arc<Mutex<Lobby>>,
    pub game: Arc<Mutex<Option<Game>>>,
    pub connections: Arc<RwLock<ws::Connections>>,
    pub supabase: SupabaseAnonClient,
}

fn new_arc_mutex<T>(item: T) -> Arc<Mutex<T>> {
    Arc::new(Mutex::new(item))
}

impl Default for AppState {
    fn default() -> Self {
        let key = std::env::var("COOKIE_SIGNING_KEY").expect("env var COOKIE_SIGNING_KEY not set");

        Self {
            cookie_signing_key: cookie::Key::from(key.as_bytes()),
            connections: Arc::new(RwLock::new(ws::Connections::default())),
            lobby: new_arc_mutex(Lobby::default()),
            game: new_arc_mutex(None),
            supabase: SupabaseAnonClient::new(),
        }
    }
}

impl FromRef<AppState> for cookie::Key {
    fn from_ref(state: &AppState) -> Self {
        state.cookie_signing_key.clone()
    }
}
