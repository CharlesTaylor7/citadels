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

fn new_arc_mutex<T>(item: T) -> Arc<Mutex<T>> {
    Arc::new(Mutex::new(item))
}

impl Default for AppState {
    fn default() -> Self {
        load_dotenv!();
        Self {
            cookie_signing_key: cookie::Key::from(env!("COOKIE_SIGNING_KEY").as_bytes()),
            connections: new_arc_mutex(ws::Connections::new()),
            lobby: new_arc_mutex(Lobby::demo(vec!["A", "B"])),
            game: new_arc_mutex(Game::default_game()),
        }
    }
}

impl FromRef<AppState> for cookie::Key {
    fn from_ref(state: &AppState) -> Self {
        state.cookie_signing_key.clone()
    }
}
