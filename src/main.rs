use citadels::server::routes::get_router;
use citadels::server::state::AppState;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    #[cfg(feature = "dotenv")]
    dotenv::dotenv().expect(".env not found");

    citadels::logger::init();

    let host = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(host).await?;

    log::info!("Listening on: {}", host);
    let state = AppState::new().await?;
    axum::serve(listener, get_router(state)).await?;

    Ok(())
}
