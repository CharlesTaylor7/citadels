use citadels_server::server::routes::get_router;

#[tokio::main]
async fn main() {
    #[cfg(feature = "dotenv")]
    dotenvy::dotenv().expect(".env not found");
    citadels::logger::init();

    let port = "0.0.0.0:8000";
    let listener = tokio::net::TcpListener::bind(port).await.unwrap();

    log::info!("Listening on port: {}", port);
    axum::serve(listener, get_router()).await.unwrap();
}
