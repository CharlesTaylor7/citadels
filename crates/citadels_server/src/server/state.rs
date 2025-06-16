use crate::server::ws;
use axum::extract::FromRef;
use axum_extra::extract::cookie;
use citadels::{game::Game, lobby::Lobby};
use sqlx::{Pool, Postgres};
use sqlx_postgres::PgPoolOptions;
use std::sync::Arc;
use tokio::sync::Mutex;

#[derive(Clone)]
pub struct AppState {
    cookie_signing_key: cookie::Key,
    pub lobby: Arc<Mutex<Lobby>>,
    pub connections: Arc<Mutex<ws::Connections>>,
    pub db: Pool<Postgres>,
}

fn new_arc_mutex<T>(item: T) -> Arc<Mutex<T>> {
    Arc::new(Mutex::new(item))
}

impl Default for AppState {
    fn default() -> Self {
        let key = std::env::var("COOKIE_SIGNING_KEY").expect("env var COOKIE_SIGNING_KEY not set");
        let database_url = std::env::var("DATABASE_URL").expect("env var DATABASE_URL not set");
        Self {
            cookie_signing_key: cookie::Key::from(key.as_bytes()),
            connections: new_arc_mutex(ws::Connections::default()),
            lobby: new_arc_mutex(Lobby::default()),
            db: PgPoolOptions::new()
                .max_connections(50)
                .connect_lazy(&database_url)
                .unwrap(),
        }
    }
}
impl FromRef<AppState> for cookie::Key {
    fn from_ref(state: &AppState) -> Self {
        state.cookie_signing_key.clone()
    }
}
