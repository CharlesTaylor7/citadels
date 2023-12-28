use citadels::lobby::Lobby;
use std::sync::{Arc, Mutex};

use axum::{
    extract::FromRef,
    routing::{get, post},
    Router,
};
use load_dotenv::load_dotenv;
use minijinja;
use tokio;
use tower_http::services::ServeDir;

use axum_extra::extract::cookie;

// TODO:
// - [x] run on 8080
// - [x] serve public/index.css
// - [x] load jinja templates
// - [x] cookies
// - routes
// - [x] /
// - [x] /register
// - [ ] /ws
// - [ ] /start

#[derive(Clone)]
pub struct Context {
    pub cookie_signing_key: cookie::Key,
    pub jinja_env: minijinja::Environment<'static>,
    pub lobby: Arc<Mutex<Lobby>>,
}

impl FromRef<Context> for cookie::Key {
    fn from_ref(context: &Context) -> Self {
        context.cookie_signing_key.clone()
    }
}

#[tokio::main]
async fn main() {
    load_dotenv!();
    let port = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(port).await.unwrap();

    println!("\nListening on port: {}", port);
    axum::serve(listener, router()).await.unwrap();
}

fn router() -> Router {
    let mut env = minijinja::Environment::new();
    use template_fragments::*;
    for (path, template) in [(
        "lobby.html",
        std::fs::read_to_string("./templates/lobby.html").unwrap(),
    )] {
        for (fragment_name, template_fragment) in split_templates(&template).unwrap() {
            env.add_template_owned(join_path(path, &fragment_name), template_fragment)
                .unwrap();
        }
    }
    let context = Context {
        cookie_signing_key: cookie::Key::from(env!("COOKIE_SIGNING_KEY").as_bytes()),
        jinja_env: env,
        lobby: Arc::new(Mutex::new(Lobby::default())),
    };

    Router::new()
        .route("/", get(handlers::index))
        .route("/register", post(handlers::register))
        .nest_service("/public", ServeDir::new("public"))
        .with_state(context)
}

fn template<'a, S: serde::Serialize>(
    env: &minijinja::Environment,
    template: &'a str,
    ctx: S,
) -> axum::response::Html<String> {
    use axum::response::Html;

    Html(env.get_template(template).unwrap().render(ctx).unwrap())
}

mod handlers {

    use std::collections::hash_map::*;

    use crate::Context;
    use axum::{extract::State, response::IntoResponse};
    use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
    use citadels::lobby::*;
    use minijinja::context;
    use uuid::Uuid;

    pub async fn index(
        state: State<Context>,
        cookies: PrivateCookieJar,
    ) -> impl axum::response::IntoResponse {
        let username = cookies
            .get("username")
            .map_or("".to_owned(), |c| c.value().to_owned());

        let player_id = cookies.get("playerId").map(|c| c.value().to_owned());

        let players: Vec<Player> = {
            let lobby = state.lobby.lock().unwrap();
            lobby
                .seating
                .iter()
                .filter_map(|id| lobby.players.get(id))
                .cloned()
                .collect()
        };

        (
            cookies.add(Cookie::new(
                "playerId",
                player_id.unwrap_or_else(|| Uuid::new_v4().to_string()),
            )),
            crate::template(
                &state.jinja_env,
                "lobby.html",
                context!(
                    username => username,
                    players => players,
                ),
            ),
        )
    }
    use serde::Deserialize;

    #[derive(Deserialize)]
    pub struct Register {
        username: String,
    }

    pub async fn register(
        state: State<Context>,
        cookies: PrivateCookieJar,
        args: axum::Form<Register>,
    ) -> impl IntoResponse {
        let cookie = cookies.get("playerId").unwrap();
        let player_id = cookie.value();
        let players: Vec<Player> = {
            let mut lobby = state.lobby.lock().unwrap();
            match lobby.players.entry(player_id.to_owned()) {
                Entry::Occupied(e) => {
                    e.into_mut().name = args.username.clone();
                }
                Entry::Vacant(e) => {
                    e.insert(Player {
                        id: player_id.to_owned(),
                        name: args.username.to_owned(),
                    });
                    lobby.seating.push(player_id.to_owned())
                }
            }
            lobby
                .seating
                .iter()
                .filter_map(|id| lobby.players.get(id))
                .cloned()
                .collect()
        };

        (
            cookies.add(Cookie::new("username", args.username.clone())),
            crate::template(
                &state.jinja_env,
                "lobby.html#players",
                context!(
                    players => players,
                ),
            ),
        )
    }
}
