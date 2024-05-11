use citadels::server::auth::refresh_sessions;
use citadels::server::routes::get_router;
use citadels::server::state::AppState;
use tokio::time::Duration;

#[tokio::main]
async fn main() {
    #[cfg(feature = "dotenv")]
    dotenv::dotenv().expect(".env not found");
    /*
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
        .init();
        */

    citadels::logger::init();

    let state = AppState::default();
    let sessions = state.sessions.clone();
    let supabase = state.supabase.clone();
    tokio::spawn(refresh_sessions(Duration::from_secs(3), sessions, supabase));

    let host = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(host).await.unwrap();

    log::info!("Listening on: {}", host);
    axum::serve(listener, get_router(state)).await.unwrap();
}
