use citadels_server::server::routes::get_router;
use poem::{listener::TcpListener, Server};

#[tokio::main]
async fn main() {
    #[cfg(feature = "dotenv")]
    dotenvy::dotenv().expect(".env not found");
    citadels::logger::init();

    let port = "0.0.0.0:8080";
    let listener = TcpListener::bind(port);

    log::info!("Listening on port: {}", port);
    let _ = Server::new(listener).run(get_router()).await;
}
