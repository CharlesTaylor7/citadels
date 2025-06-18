use crate::server::ws;
use citadels::lobby::Lobby;
use sqlx::{Pool, Postgres};
use sqlx_postgres::PgPoolOptions;
use std::sync::Arc;
use tokio::sync::Mutex;

#[derive(Clone)]
pub struct AppState {
    pub lobby: Arc<Mutex<Lobby>>,
    pub connections: Arc<Mutex<ws::Connections>>,
    pub db: Pool<Postgres>,
}

fn new_arc_mutex<T>(item: T) -> Arc<Mutex<T>> {
    Arc::new(Mutex::new(item))
}

impl Default for AppState {
    fn default() -> Self {
        let database_url = std::env::var("DATABASE_URL").expect("env var DATABASE_URL not set");
        Self {
            connections: new_arc_mutex(ws::Connections::default()),
            lobby: new_arc_mutex(Lobby::default()),
            db: PgPoolOptions::new()
                .max_connections(50)
                .connect_lazy(&database_url)
                .unwrap(),
        }
    }
}
