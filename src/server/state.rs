use crate::server::ws;
use crate::types::PlayerId;
use crate::{game::Game, lobby::Lobby};
use axum::extract::FromRef;
use axum_extra::extract::cookie;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock, Weak};

#[derive(Clone)]
pub struct AppState {
    cookie_signing_key: cookie::Key,
    pub rooms: Arc<RwLock<Vec<Arc<Mutex<Room>>>>>,
    pub connections: Arc<Mutex<ws::Connections>>,
    pub lookup: HashMap<PlayerId, Weak<Room>>,
}

pub enum Room {
    Lobby(Lobby),
    Game(Game),
}

impl Default for AppState {
    fn default() -> Self {
        let key = std::env::var("COOKIE_SIGNING_KEY").expect("env var COOKIE_SIGNING_KEY not set");
        Self {
            cookie_signing_key: cookie::Key::from(key.as_bytes()),
            connections: Arc::new(Mutex::new(ws::Connections::default())),
            rooms: Arc::new(RwLock::new(Vec::new())),
            lookup: HashMap::new(),
            //    games: Arc::new(RwLock::new(Vec::new())),
            //   lobbies: Arc::new(RwLock::new(Vec::new())),
        }
    }
}

impl FromRef<AppState> for cookie::Key {
    fn from_ref(state: &AppState) -> Self {
        state.cookie_signing_key.clone()
    }
}
