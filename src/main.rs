use citadels::server::routes::get_router;
use log::*;
use tokio;

#[tokio::main]
async fn main() {
    citadels::logger::init();

    let port = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(port).await.unwrap();

    info!("Listening on port: {}", port);
    axum::serve(listener, get_router()).await.unwrap();
}
