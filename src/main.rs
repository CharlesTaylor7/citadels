use citadels::server::routes::get_router;

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

    let port = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(port).await.unwrap();

    log::info!("Listening on port: {}", port);
    axum::serve(listener, get_router()).await.unwrap();
}
