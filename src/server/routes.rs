use crate::actions::ActionSubmission;
use crate::game::Game;
use crate::server::state::AppState;
use crate::templates::GameTemplate;
use crate::templates::*;
use crate::types::PlayerName;
use askama::Template;
use axum::extract::State;
use axum::response::{ErrorResponse, Html, Redirect, Response, Result};
use axum::routing::{get, post};
use axum::Router;
use axum::{extract::ws::WebSocketUpgrade, response::IntoResponse};
use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
use http::StatusCode;
use log::*;
use serde::Deserialize;
use std::borrow::Borrow;
use std::mem;
use tower_http::services::ServeDir;
use uuid::Uuid;

pub fn get_router() -> Router {
    let context = AppState::default();

    let router = Router::new()
        .route("/", get(index))
        .route("/lobby", get(get_lobby))
        .route("/lobby/register", post(register))
        .route("/ws", get(get_ws))
        .route("/game", get(game))
        .route("/game/actions", get(get_game_actions))
        .route("/game/actions/:action", get(get_game_action_menu))
        .route("/game/city/:player_name", get(get_game_city))
        .route("/game/logs", get(get_game_logs))
        .route("/game", post(start))
        .route("/game/action", post(perform_game_action));

    extend_router(router)
        .nest_service("/public", ServeDir::new("public"))
        .with_state(context)
}

#[cfg(feature = "dev")]
fn extend_router(router: Router<AppState>) -> Router<AppState> {
    router.route("/game/impersonate", post(game_impersonate))
}

#[cfg(not(feature = "dev"))]
fn extend_router(router: Router<AppState>) -> Router<AppState> {
    router
}

pub async fn index() -> impl IntoResponse {
    Redirect::to("/lobby")
}

pub async fn get_lobby(app: State<AppState>, cookies: PrivateCookieJar) -> impl IntoResponse {
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

pub async fn get_game_actions(
    _app: State<AppState>,
    _cookies: PrivateCookieJar,
) -> Result<Html<String>, ErrorResponse> {
    todo!()
}

pub async fn get_game_city(
    _app: State<AppState>,
    _cookies: PrivateCookieJar,
) -> Result<Html<String>, ErrorResponse> {
    todo!()
}

pub async fn get_game_logs(
    _app: State<AppState>,
    _cookies: PrivateCookieJar,
) -> Result<Html<String>, ErrorResponse> {
    todo!()
}

pub async fn get_game_action_menu(
    _app: State<AppState>,
    _cookies: PrivateCookieJar,
) -> Result<Html<String>, ErrorResponse> {
    todo!()
}

pub async fn get_ws(
    state: State<AppState>,
    cookies: PrivateCookieJar,
    ws: WebSocketUpgrade,
) -> impl IntoResponse {
    if let Some(cookie) = cookies.get("player_id") {
        ws.on_upgrade(move |socket| {
            crate::server::ws::handle_socket(state, cookie.value().to_owned(), socket)
        })
        .into_response()
    } else {
        StatusCode::BAD_REQUEST.into_response()
    }
}

async fn perform_game_action(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    action: axum::Json<ActionSubmission>,
) -> Result<Response> {
    let cookie = cookies.get("player_id").ok_or("missing cookie")?;
    let mut game = app.game.lock().unwrap();
    let game = game.as_mut().ok_or("game hasn't started")?;

    let active_player = game.active_player()?;

    if cfg!(not(feature = "dev")) && cookie.value() != active_player.id {
        return Err((StatusCode::BAD_REQUEST, "not your turn!").into());
    }

    info!("{:#?}", action.0);
    match action.0 {
        ActionSubmission::Complete(action) => {
            //
            match game.perform(action) {
                Ok(()) => {
                    let g = &game;
                    app.connections
                        .lock()
                        .unwrap()
                        .broadcast_each(move |id| GameTemplate::render(g, Some(id), None));

                    Ok(StatusCode::OK.into_response())
                }
                Err(error) => Err((StatusCode::BAD_REQUEST, error).into()),
            }
        }

        ActionSubmission::Incomplete { action: _ } => {
            //
            Ok(StatusCode::OK.into_response())
        }
    }
}

#[cfg(feature = "dev")]
#[derive(Deserialize)]
pub struct Impersonate {
    name: PlayerName,
}

#[cfg(feature = "dev")]
async fn game_impersonate(
    app: State<AppState>,
    body: axum::Json<Impersonate>,
) -> Result<Response, ErrorResponse> {
    let mut game = app.game.lock().unwrap();
    let game = game.as_mut().ok_or("game hasn't started")?;
    let html = GameTemplate::render(game, None, Some(body.name.borrow()))?;
    app.connections.lock().unwrap().broadcast(html);

    Ok(StatusCode::OK.into_response())
}
