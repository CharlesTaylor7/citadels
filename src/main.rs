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
        self.0.values_mut().for_each(|ws| {
            let _ = ws.send(Ok(Message::Text(html.0.clone())));
        });
    }

    pub fn broadcast_each<'a, F>(&'a mut self, to_html: F)
    where
        F: Fn(&'a str) -> Option<Html<String>>,
    {
        self.0.iter_mut().for_each(|(key, ws)| {
            if let Some(html) = to_html(key) {
                let _ = ws.send(Ok(Message::Text(html.0.clone())));
            }
        });
    }
}
pub enum AppError {
    Default,
}
#[derive(Clone)]
pub struct AppState {
    cookie_signing_key: cookie::Key,
    pub lobby: Arc<Mutex<Lobby>>,
    pub game: Arc<Mutex<Option<Game>>>,
    pub connections: Arc<Mutex<Connections>>,
}

impl AppState {
    #[cfg(debug_assertions)]
    pub fn default_game() -> Option<Game> {
        Some(Game::start(Lobby::demo(vec![
            "Alph", "Brittany", "Charlie",
        ])))
    }

    #[cfg(not(debug_assertions))]
    pub fn default_game() -> Option<Game> {
        None
    }
}

impl Default for AppState {
    fn default() -> Self {
        load_dotenv!();
        Self {
            cookie_signing_key: cookie::Key::from(env!("COOKIE_SIGNING_KEY").as_bytes()),
            connections: Arc::new(Mutex::new(Connections(HashMap::new()))),
            lobby: Arc::new(Mutex::new(Lobby::default())),
            game: Arc::new(Mutex::new(AppState::default_game())),
        }
    }
}

impl FromRef<AppState> for cookie::Key {
    fn from_ref(state: &AppState) -> Self {
        state.cookie_signing_key.clone()
    }
}

#[tokio::main]
async fn main() {
    let port = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(port).await.unwrap();

    println!("\nListening on port: {}", port);
    axum::serve(listener, router()).await.unwrap();
}

fn router() -> Router {
    let context = AppState::default();

    Router::new()
        .route("/", get(handlers::index))
        .route("/lobby", get(handlers::lobby))
        .route("/register", post(handlers::register))
        .route("/ws", get(handlers::ws))
        .route("/game", get(handlers::game))
        .route("/game", post(handlers::start))
        .route("/game/:path", get(handlers::game_impersonate))
        .nest_service("/public", ServeDir::new("public"))
        .with_state(context)
}

mod handlers {
    use crate::{AppError, AppState};
    use askama::Template;
    use axum::extract::{Path, State};
    use axum::response::{Html, Redirect, Response};
    use axum::{extract::ws::WebSocketUpgrade, response::IntoResponse};
    use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
    use citadels::types::District;
    use citadels::{game, lobby};
    use http::StatusCode;
    use serde::Deserialize;
    use std::mem;
    use uuid::Uuid;

    #[cfg(debug_assertions)]
    pub async fn index(cookies: PrivateCookieJar) -> impl IntoResponse {
        (
            cookies.add(Cookie::new("playerId", "Alph")),
            Redirect::to("/lobby"),
        )
    }

    #[cfg(not(debug_assertions))]
    pub async fn index(cookies: PrivateCookieJar) -> impl IntoResponse {
        Redirect::to("/lobby")
    }
    pub async fn lobby(app: State<AppState>, cookies: PrivateCookieJar) -> impl IntoResponse {
        if app.game.lock().unwrap().is_some() {
            return Redirect::to("/game").into_response();
        }

        let username = cookies
            .get("username")
            .map_or("".to_owned(), |c| c.value().to_owned());

        let player_id = cookies.get("playerId").map(|c| c.value().to_owned());

        let lobby = app.lobby.lock().unwrap();

        (
            cookies.add(Cookie::new(
                "playerId",
                player_id.unwrap_or_else(|| Uuid::new_v4().to_string()),
            )),
            Html(
                LobbyTemplate {
                    username: &username,
                    players: &lobby.players,
                }
                .render()
                .unwrap(),
            ),
        )
            .into_response()
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
        let cookie = cookies.get("playerId").unwrap();
        let player_id = cookie.value();
        let mut lobby = app.lobby.lock().unwrap();
        lobby.register(player_id, &args.username);

        let html = Html(
            LobbyPlayersTemplate {
                players: &lobby.players,
            }
            .render()
            .unwrap(),
        );
        app.connections.lock().unwrap().broadcast(html);

        cookies.add(Cookie::new("username", args.username.clone()))
    }
    pub async fn start(app: State<AppState>, _cookies: PrivateCookieJar) -> impl IntoResponse {
        {
            let mut game = app.game.lock().unwrap();
            if game.is_some() {
                return (
                    StatusCode::BAD_REQUEST,
                    "can't overwrite a game in progress",
                )
                    .into_response();
            }
            let mut lobby = app.lobby.lock().unwrap();
            if lobby.players.is_empty() {
                return (StatusCode::BAD_REQUEST, "can't start an empty game").into_response();
            }

            // Start the game, and remove all players from the lobby
            *game = Some(Game::start(mem::take(&mut lobby)));
        }

        if let Some(game) = app.game.lock().unwrap().as_ref() {
            app.connections
                .lock()
                .unwrap()
                .broadcast_each(move |id| GameTemplate::render(game, id));
        }

        StatusCode::OK.into_response()
    }

    fn game_response(app: State<AppState>, cookies: PrivateCookieJar) -> Option<Html<String>> {
        let id = cookies.get("playerId")?;
        let game = app.game.lock().unwrap();
        let game = game.as_ref()?;
        GameTemplate::render(game, id.value())
    }

    pub async fn game(app: State<AppState>, cookies: PrivateCookieJar) -> impl IntoResponse {
        game_response(app, cookies).map_or(Redirect::to("/lobby").into_response(), |html| {
            html.into_response()
        })
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
            StatusCode::BAD_REQUEST.into_response()
        }
    }

    #[cfg(debug_assertions)]
    pub async fn game_impersonate(
        app: State<AppState>,
        cookies: PrivateCookieJar,
        path: Path<String>,
    ) -> impl IntoResponse {
        let cookies = cookies.add(Cookie::new("playerId", path.0));
        game(app, cookies).await
    }

    #[cfg(not(debug_assertions))]
    pub async fn game_impersonate(
        app: state<appstate>,
        cookies: privatecookiejar,
        path: path<string>,
    ) -> impl IntoResponse {
        Redirect::to("/game")
    }
}

pub mod ws {
    use crate::templates::*;
    use crate::AppState;
    use axum::extract::ws::{Message, WebSocket};
    use axum::extract::State;
    use futures::stream::StreamExt;
    use std::ops::ControlFlow;
    use tokio::sync::mpsc::{self};
    use tokio_stream::wrappers::UnboundedReceiverStream;

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
