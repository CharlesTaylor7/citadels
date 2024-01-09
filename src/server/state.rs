use crate::server::ws;
use crate::{game::Game, lobby::Lobby};
use axum::extract::FromRef;
use axum_extra::extract::cookie;
use load_dotenv::load_dotenv;

use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct AppState {
    cookie_signing_key: cookie::Key,
    pub lobby: Arc<Mutex<Lobby>>,
    pub game: Arc<Mutex<Option<Game>>>,
    pub connections: Arc<Mutex<ws::Connections>>,
}

impl Default for AppState {
    fn default() -> Self {
        load_dotenv!();
        Self {
            cookie_signing_key: cookie::Key::from(env!("COOKIE_SIGNING_KEY").as_bytes()),
            connections: Arc::new(Mutex::new(ws::Connections::new())),
            lobby: Arc::new(Mutex::new(Lobby::default())),
            game: Arc::new(Mutex::new(Game::default_game())),
        }
    }
}

impl FromRef<AppState> for cookie::Key {
    fn from_ref(state: &AppState) -> Self {
        state.cookie_signing_key.clone()
    }
}
