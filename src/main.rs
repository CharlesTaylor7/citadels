use axum::{routing::get, Router};
use minijinja;
use tokio;
use tower_http::services::ServeDir;

// TODO:
// - [x] run on 8080
// - [x] serve public/index.css
// - [ ] load jinja templates
// - routes
// - [ ] /
// - [ ] /ws
// - [ ] /register
// - [ ] /start

#[derive(Clone)]
pub struct Context {
    pub jinja_env: minijinja::Environment<'static>,
}

#[tokio::main]
async fn main() {
    let port = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(port).await.unwrap();

    println!("\nListening on port: {}", port);
    axum::serve(listener, router()).await.unwrap();
}

fn router() -> Router {
    let mut env = minijinja::Environment::new();
    env.add_template_owned(
        "lobby.html",
        std::fs::read_to_string("./templates/lobby.html").unwrap(),
    )
    .unwrap();
    let context = Context { jinja_env: env };

    Router::new()
        .route("/", get(handlers::index))
        .nest_service("/public", ServeDir::new("public"))
        .with_state(context)
}

mod handlers {
    use crate::Context;
    use axum::{
        extract::State,
        response::{Html, IntoResponse, Response},
    };
    use minijinja::context;

    pub async fn index(context: State<Context>) -> Response {
        Html(
            context
                .jinja_env
                .get_template("lobby.html")
                .unwrap()
                .render(context!(name => "World"))
                .unwrap(),
        )
        .into_response()
    }
}
