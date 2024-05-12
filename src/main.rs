use citadels::server::routes::get_router;
use citadels::server::state::AppState;

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

    let host = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(host).await.unwrap();

    log::info!("Listening on: {}", host);
    let state = AppState::default();
    axum::serve(listener, get_router(state)).await.unwrap();
}
