use axum::{
    routing::get,
    Router,
};
use tokio;
use tower_http::services::ServeDir;

// TODO: 
// - [x] run on 8080
// - [ ] serve public/index.css
// - [ ] /
// - [ ] /ws
// - [ ] /register
// - [ ] /start
    

#[tokio::main]
async fn main() {
    let port = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(port).await.unwrap();
    println!("\nListening on port: {}", port);
    axum::serve(listener, router()).await.unwrap();
}

fn router() -> Router {
    // serve the file in the "assets" directory under `/assets`
    Router::new()
        .route("/", get( || async { "Hello, World!" }))
        .nest_service("/public", ServeDir::new("public"))
}
