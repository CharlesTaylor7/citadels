use axum::{
    extract::{ws::Message, FromRef},
    response::{ErrorResponse, Html},
    routing::{get, post},
    Error, Router,
};
use axum_extra::extract::cookie;
use citadels::{game::Game, lobby::Lobby};
use load_dotenv::load_dotenv;
use log::*;
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
        F: Fn(&'a str) -> Result<Html<String>, ErrorResponse>,
    {
        for (key, ws) in self.0.iter_mut() {
            match to_html(key) {
                Ok(html) => {
                    let _ = ws.send(Ok(Message::Text(html.0.clone())));
                }
                Err(e) => debug!("{:#?}", e),
            }
        }
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

impl Default for AppState {
    fn default() -> Self {
        load_dotenv!();
        Self {
            cookie_signing_key: cookie::Key::from(env!("COOKIE_SIGNING_KEY").as_bytes()),
            connections: Arc::new(Mutex::new(Connections(HashMap::new()))),
            lobby: Arc::new(Mutex::new(Lobby::default())),
            game: Arc::new(Mutex::new(Game::default_game())),
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
    citadels::logger::init();

    let port = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(port).await.unwrap();

    info!("Listening on port: {}", port);
    axum::serve(listener, get_router()).await.unwrap();
}

fn get_router() -> Router {
    let context = AppState::default();

    let router = Router::new()
        .route("/", get(handlers::index))
        .route("/lobby", get(handlers::lobby))
        .route("/register", post(handlers::register))
        .route("/ws", get(handlers::ws))
        .route("/game", get(handlers::game))
        .route("/game", post(handlers::start))
        .route("/game/action", post(handlers::game_action));

    extend_router(router)
        .nest_service("/public", ServeDir::new("public"))
        .with_state(context)
}

#[cfg(feature = "dev")]
fn extend_router(router: Router<AppState>) -> Router<AppState> {
    router.route("/game/impersonate", post(handlers::game_impersonate))
}

#[cfg(not(feature = "dev"))]
fn extend_router(router: Router<AppState>) -> Router<AppState> {
    router
}

mod handlers {
    use crate::AppState;
    use askama::Template;
    use axum::extract::State;
    use axum::response::{ErrorResponse, Html, Redirect, Response, Result};
    use axum::{extract::ws::WebSocketUpgrade, response::IntoResponse};
    use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
    use citadels::actions::Action;
    use citadels::game::Game;
    use citadels::templates::*;
    use citadels::types::PlayerName;
    use http::StatusCode;
    use log::*;
    use serde::Deserialize;
    use std::borrow::Borrow;
    use std::mem;
    use uuid::Uuid;

    pub async fn index() -> impl IntoResponse {
        Redirect::to("/lobby")
    }

    pub async fn lobby(app: State<AppState>, cookies: PrivateCookieJar) -> impl IntoResponse {
        let player_id = cookies.get("player_id").map(|c| c.value().to_owned());
        let cookies = cookies.add(Cookie::new(
            "player_id",
            player_id.unwrap_or_else(|| Uuid::new_v4().to_string()),
        ));

        if app.game.lock().unwrap().is_some() {
            return (cookies, Redirect::to("/game")).into_response();
        }

        let username = cookies
            .get("username")
            .map_or("".to_owned(), |c| c.value().to_owned());

        let lobby = app.lobby.lock().unwrap();

        (
            cookies,
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
        args: axum::Json<Register>,
    ) -> impl IntoResponse {
        let cookie = cookies.get("player_id").unwrap();
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
                .broadcast_each(move |id| GameTemplate::render(game, Some(id), None));
            return StatusCode::OK.into_response();
        }

        StatusCode::BAD_REQUEST.into_response()
    }

    pub async fn game(
        app: State<AppState>,
        cookies: PrivateCookieJar,
    ) -> Result<Html<String>, ErrorResponse> {
        let cookie = cookies.get("player_id");
        let id = cookie.as_ref().map(|c| c.value());
        let game = app.game.lock().unwrap();
        let game = game.as_ref();
        if let Some(game) = game.as_ref() {
            GameTemplate::render(game, id, None)
        } else {
            Err(ErrorResponse::from(Redirect::to("/lobby")))
        }
    }

    pub async fn ws(
        state: State<AppState>,
        cookies: PrivateCookieJar,
        ws: WebSocketUpgrade,
    ) -> impl IntoResponse {
        if let Some(cookie) = cookies.get("player_id") {
            ws.on_upgrade(move |socket| {
                crate::ws::handle_socket(state, cookie.value().to_owned(), socket)
            })
            .into_response()
        } else {
            StatusCode::BAD_REQUEST.into_response()
        }
    }
    pub async fn game_action(
        app: State<AppState>,
        cookies: PrivateCookieJar,
        action: axum::Json<Action>,
    ) -> Result<Response> {
        let cookie = cookies.get("player_id").ok_or("missing cookie")?;
        let mut game = app.game.lock().unwrap();
        let game = game.as_mut().ok_or("game hasn't started")?;

        let active_player = game.active_player()?;

        if cfg!(not(feature = "dev")) && cookie.value() != active_player.id {
            return Err((StatusCode::BAD_REQUEST, "not your turn!").into());
        }

        match game.perform(action.0) {
            Ok(()) => {
                let g = &game;
                app.connections
                    .lock()
                    .unwrap()
                    .broadcast_each(move |id| GameTemplate::render(g, Some(id), None));

                debug!("Draft: {:#?}", game.draft);
                debug!("{:#?}", game.logs);
                debug!("Turn: {:#?}", game.active_turn);
                debug!("Active: {:#?}", game.active_player());
                Ok(StatusCode::OK.into_response())
            }
            Err(error) => Err((StatusCode::BAD_REQUEST, error).into()),
        }
    }

    #[cfg(feature = "dev")]
    #[derive(Deserialize)]
    pub struct Impersonate {
        name: PlayerName,
    }

    #[cfg(feature = "dev")]
    pub async fn game_impersonate(
        app: State<AppState>,
        body: axum::Json<Impersonate>,
    ) -> Result<Response, ErrorResponse> {
        let mut game = app.game.lock().unwrap();
        let game = game.as_mut().ok_or("game hasn't started")?;
        let html = GameTemplate::render(game, None, Some(body.name.borrow()))?;
        app.connections.lock().unwrap().broadcast(html);

        Ok(StatusCode::OK.into_response())
    }
}

pub mod ws {
    use crate::AppState;
    use axum::extract::ws::{Message, WebSocket};
    use axum::extract::State;
    use futures::stream::StreamExt;
    use log::*;
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

        info!("WS - connected");

        while let Some(Ok(msg)) = ws_recv.next().await {
            if process_message(msg).is_err() {
                break;
            }
        }
    }

    fn process_message(msg: Message) -> Result<(), ()> {
        match msg {
            Message::Text(t) => {
                debug!("WS - client sent str: {t:?}");
            }
            Message::Binary(d) => {
                debug!("WS - client sent {} bytes: {:?}", d.len(), d);
            }
            Message::Close(_) => {
                debug!("WS - closed connection");
                return Err(());
            }

            // axum automatically replies to ping
            Message::Ping(_) => {
                trace!("WS - Ping")
            }
            Message::Pong(_) => {
                trace!("WS - Pong")
            }
        }
        Ok(())
    }
}
