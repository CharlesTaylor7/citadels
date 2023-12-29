use axum::{
    extract::{ws::Message, FromRef},
    response::Html,
    routing::{get, post},
    Error, Router,
};
use axum_extra::extract::cookie;
use citadels::{
    game::{self, Game},
    lobby::Lobby,
};
use load_dotenv::load_dotenv;
use std::collections::hash_map::HashMap;
use std::sync::{Arc, Mutex};
use tokio::{self, sync::mpsc};
use tower_http::services::ServeDir;

type WebSocketSink = mpsc::UnboundedSender<Result<Message, Error>>;
pub struct Connections(pub HashMap<String, WebSocketSink>);

impl Connections {
    pub fn broadcast(&mut self, html: Html<String>) {
        self.0.iter_mut().for_each(|(_key, ws)| {
            let _ = ws.send(Ok(Message::Text(html.0.clone())));
        });
    }
}

#[derive(Clone)]
pub struct AppState {
    pub cookie_signing_key: cookie::Key,
    pub lobby: Arc<Mutex<Lobby>>,

    // TODO: multi game support
    pub game: Arc<Mutex<Option<Game>>>,
    pub connections: Arc<Mutex<Connections>>,
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
    let context = AppState {
        cookie_signing_key: cookie::Key::from(env!("COOKIE_SIGNING_KEY").as_bytes()),
        lobby: Arc::new(Mutex::new(Lobby::default())),
        game: Arc::new(Mutex::new(Option::None)),
        connections: Arc::new(Mutex::new(Connections(HashMap::new()))),
    };

    Router::new()
        .route("/", get(handlers::index))
        .route("/register", post(handlers::register))
        .route("/ws", get(handlers::ws))
        .route("/game", get(handlers::game))
        .route("/game", post(handlers::start))
        .nest_service("/public", ServeDir::new("public"))
        .with_state(context)
}

mod handlers {
    use crate::AppState;
    use askama::Template;
    use axum::extract::State;
    use axum::response::Html;
    use axum::{extract::ws::WebSocketUpgrade, response::IntoResponse};
    use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
    use citadels::lobby::*;
    use citadels::{game, lobby};
    use http::StatusCode;
    use serde::Deserialize;
    use std::collections::hash_map::*;
    use std::mem;
    use uuid::Uuid;

    #[derive(Template)]
    #[template(path = "lobby/index.html")]
    struct LobbyTemplate<'a> {
        username: &'a str,
        players: Vec<&'a lobby::Player>,
    }

    #[derive(Template)]
    #[template(path = "lobby/players.html")]
    struct LobbyPlayersTemplate<'a> {
        players: Vec<&'a lobby::Player>,
    }

    pub async fn index(app: State<AppState>, cookies: PrivateCookieJar) -> impl IntoResponse {
        let username = cookies
            .get("username")
            .map_or("".to_owned(), |c| c.value().to_owned());

        let player_id = cookies.get("playerId").map(|c| c.value().to_owned());

        let lobby = app.lobby.lock().unwrap();
        let players: Vec<&Player> = lobby
            .seating
            .iter()
            .filter_map(|id| lobby.players.get(id))
            .collect();

        (
            cookies.add(Cookie::new(
                "playerId",
                player_id.unwrap_or_else(|| Uuid::new_v4().to_string()),
            )),
            Html(
                LobbyTemplate {
                    username: &username,
                    players,
                }
                .render()
                .unwrap(),
            ),
        )
    }

    #[derive(Deserialize)]
    pub struct Register {
        username: String,
    }

    pub async fn register(
        app: State<AppState>,
        cookies: PrivateCookieJar,
        args: axum::Form<Register>,
    ) -> impl IntoResponse {
        println!("register!");
        let cookie = cookies.get("playerId").unwrap();
        let player_id = cookie.value();
        let mut lobby = app.lobby.lock().unwrap();
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

        let players: Vec<&Player> = lobby
            .seating
            .iter()
            .filter_map(|id| lobby.players.get(id))
            .collect();

        let html = Html(LobbyPlayersTemplate { players }.render().unwrap());
        app.connections.lock().unwrap().broadcast(html);

        (
            cookies.add(Cookie::new("username", args.username.clone())),
            "",
        )
    }

    #[derive(Template)]
    #[template(path = "game/index.html")]
    struct GameTemplate<'a> {
        players: Vec<&'a game::Player>,
    }

    use crate::game::Game;
    impl<'a> GameTemplate<'a> {
        pub fn from(_game: &'a Game) -> GameTemplate<'a> {
            todo!()
        }
    }

    pub async fn start(app: State<AppState>, _cookies: PrivateCookieJar) -> impl IntoResponse {
        {
            let mut game = app.game.lock().unwrap();
            // can't over write a game in progress
            if game.is_some() {
                return (StatusCode::BAD_REQUEST, "").into_response();
            }
            let mut lobby = app.lobby.lock().unwrap();
            if lobby.seating.is_empty() {
                // can't start an empty game
                return (StatusCode::BAD_REQUEST, "").into_response();
            }

            // Start the game, and remove all players from the lobby
            *game = Some(Game::new(mem::take(&mut lobby)));
        }

        use axum::response::Html;
        if let Some(game) = app.game.lock().unwrap().as_mut() {
            let html = Html(GameTemplate::from(game).render().unwrap());

            app.connections.lock().unwrap().broadcast(html.clone());

            return html.into_response();
        }
        return (StatusCode::BAD_REQUEST, "").into_response();
    }

    pub async fn game(_app: State<AppState>, _cookies: PrivateCookieJar) -> impl IntoResponse {
        //app.template("game.html", context!())
        //
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
            .0
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
