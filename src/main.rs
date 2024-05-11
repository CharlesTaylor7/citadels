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
    tokio::spawn(state.clone().refresh_sessions(
        // refresh every 30 minutes
        Duration::from_secs(30 * 60),
    ));

    let host = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(host).await.unwrap();

    log::info!("Listening on: {}", host);
    axum::serve(listener, get_router(state)).await.unwrap();
}
