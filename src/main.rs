use axum::{
    extract::{ws::Message, FromRef},
    response::Html,
    routing::{get, post},
    Error, Router,
};
use axum_extra::extract::cookie;
use citadels::lobby::Lobby;
use load_dotenv::load_dotenv;
use minijinja;
use std::sync::{Arc, Mutex};
use std::{collections::hash_map::HashMap, sync::RwLock};
use tokio::{self, sync::mpsc};
use tower_http::services::ServeDir;

// TODO:
// - [ ] /start

type WebSocketSink = mpsc::UnboundedSender<Result<Message, Error>>;

#[derive(Clone)]
pub struct AppState {
    pub cookie_signing_key: cookie::Key,
    pub jinja_env: minijinja::Environment<'static>,
    pub lobby: Arc<RwLock<Lobby>>,
    pub connections: Arc<Mutex<HashMap<String, WebSocketSink>>>,
}

impl AppState {
    fn template<'a, S: serde::Serialize>(&self, template: &'a str, ctx: S) -> Html<String> {
        Html(
            self.jinja_env
                .get_template(template)
                .unwrap()
                .render(ctx)
                .unwrap(),
        )
    }

    pub fn broadcast(&mut self, html: Html<String>) {
        self.connections
            .lock()
            .unwrap()
            .iter_mut()
            .for_each(|(_key, ws)| {
                let _ = ws.send(Ok(Message::Text(html.0.clone())));
            });
    }
}

impl FromRef<AppState> for cookie::Key {
    fn from_ref(state: &AppState) -> Self {
        state.cookie_signing_key.clone()
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
    let context = AppState {
        cookie_signing_key: cookie::Key::from(env!("COOKIE_SIGNING_KEY").as_bytes()),
        jinja_env: env,
        lobby: Arc::new(RwLock::new(Lobby::default())),
        connections: Arc::new(Mutex::new(HashMap::new())),
    };

    Router::new()
        .route("/", get(handlers::index))
        .route("/register", post(handlers::register))
        .route("/ws", get(handlers::ws))
        .route("/start", get(handlers::start))
        .nest_service("/public", ServeDir::new("public"))
        .with_state(context)
}

mod handlers {

    use crate::AppState;

    use axum::extract::State;

    use axum::{extract::ws::WebSocketUpgrade, response::IntoResponse};
    use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};

    use citadels::lobby::*;
    use http::StatusCode;
    use minijinja::context;
    use std::collections::hash_map::*;

    use uuid::Uuid;

    pub async fn index(
        app: State<AppState>,
        cookies: PrivateCookieJar,
    ) -> impl axum::response::IntoResponse {
        let username = cookies
            .get("username")
            .map_or("".to_owned(), |c| c.value().to_owned());

        let player_id = cookies.get("playerId").map(|c| c.value().to_owned());

        let players: Vec<Player> = {
            let lobby = app.lobby.read().unwrap();
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
            app.template(
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
        mut app: State<AppState>,
        cookies: PrivateCookieJar,
        args: axum::Form<Register>,
    ) -> impl IntoResponse {
        println!("register!");
        let cookie = cookies.get("playerId").unwrap();
        let player_id = cookie.value();
        let players: Vec<Player> = {
            let mut lobby = app.lobby.write().unwrap();
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
        let html = app.template(
            "lobby.html#players",
            context!(
                players => players,
            ),
        );
        app.broadcast(html);

        (
            cookies.add(Cookie::new("username", args.username.clone())),
            "",
        )
    }

    pub async fn start(_state: State<AppState>, _cookies: PrivateCookieJar) -> impl IntoResponse {
        (StatusCode::NOT_FOUND, "wip")
    }

    pub async fn ws(
        state: State<AppState>,
        cookies: PrivateCookieJar,
        ws: WebSocketUpgrade,
    ) -> impl IntoResponse {
        if let Some(cookie) = cookies.get("playerId") {
            ws.on_upgrade(move |socket| {
                crate::ws::handle_socket(state, cookie.value().to_owned(), socket)
            })
            .into_response()
        } else {
            (StatusCode::BAD_REQUEST, "bad request").into_response()
        }
    }
}

pub mod ws {
    use axum::extract::ws::{Message, WebSocket};
    use axum::extract::State;
    use futures::stream::StreamExt;
    use tokio::sync::mpsc::{self};
    use tokio_stream::wrappers::UnboundedReceiverStream;

    use std::ops::ControlFlow;

    use crate::AppState;
    pub async fn handle_socket(state: State<AppState>, player_id: String, socket: WebSocket) {
        let (ws_sender, mut ws_recv) = socket.split();
        let (chan_sender, chan_recv) = mpsc::unbounded_channel();
        tokio::spawn(UnboundedReceiverStream::new(chan_recv).forward(ws_sender));

        state
            .connections
            .lock()
            .unwrap()
            .insert(player_id, chan_sender);

        while let Some(Ok(msg)) = ws_recv.next().await {
            if process_message(msg).is_break() {
                break;
            }
        }
    }

    fn process_message(msg: Message) -> ControlFlow<(), ()> {
        match msg {
            Message::Text(t) => {
                println!(">>> sent str: {t:?}");
            }
            Message::Binary(d) => {
                println!(">>> sent {} bytes: {:?}", d.len(), d);
            }
            Message::Close(c) => {
                if let Some(cf) = c {
                    println!(
                        ">>> sent close with code {} and reason `{}`",
                        cf.code, cf.reason
                    );
                } else {
                    println!(">>> sent close message without CloseFrame");
                }
                return ControlFlow::Break(());
            }

            // axum's automatically replies to ping
            Message::Ping(_) => {}
            Message::Pong(_) => {}
        }
        ControlFlow::Continue(())
    }
}
