use crate::actions::{ActionSubmission, ActionTag};
use crate::game::Game;
use crate::roles::Rank;
use crate::server::state::AppState;
use crate::templates::GameTemplate;
use crate::templates::*;
use crate::types::{Marker, PlayerName};
use askama::Template;
use axum::extract::{Path, State};
use axum::response::{ErrorResponse, Html, Redirect, Response, Result};
use axum::routing::{get, post};
use axum::Router;
use axum::{extract::ws::WebSocketUpgrade, response::IntoResponse};
use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
use http::StatusCode;
use rand_core::SeedableRng;
use serde::Deserialize;
use std::borrow::{Borrow, Cow};
use std::mem;
use time::Duration;
use tower_http::services::ServeDir;
use uuid::Uuid;

pub fn get_router() -> Router {
    let context = AppState::default();

    Router::new()
        .route("/", get(index))
        .route("/lobby", get(get_lobby))
        .route("/lobby/register", post(register))
        .route("/ws", get(get_ws))
        .route("/game", get(game))
        .route("/game/actions", get(get_game_actions))
        .route("/game/city/:player_name", get(get_game_city))
        .route("/game/logs", get(get_game_logs))
        .route("/game", post(start))
        .route("/game/action", post(submit_game_action))
        .route("/game/menu/:menu", get(get_game_menu))
        .nest_service("/public", ServeDir::new("public"))
        .with_state(context)
}

pub async fn index() -> impl IntoResponse {
    Redirect::to("/lobby")
}

pub async fn get_lobby(app: State<AppState>, mut cookies: PrivateCookieJar) -> impl IntoResponse {
    let player_id = cookies.get("player_id");

    if player_id.is_none() {
        log::info!("Setting new player_id cookie with 1 week expiry");
        let id = Uuid::new_v4().to_string();
        let cookie = Cookie::build(("player_id", id)).max_age(Duration::WEEK);
        cookies = cookies.add(cookie);
    }

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
    let mut lobby = app.lobby.lock().unwrap();
    if lobby.players.len() < 2 {
        return (StatusCode::BAD_REQUEST, "too few players to start a game").into_response();
    }

    if lobby.players.len() > 8 {
        return (StatusCode::BAD_REQUEST, "too many players to start a game").into_response();
    }

    let mut game = app.game.lock().unwrap();
    if game.is_some() {
        return (
            StatusCode::BAD_REQUEST,
            "can't overwrite a game in progress",
        )
            .into_response();
    }

    // Start the game, and remove all players from the lobby
    *game = Some(Game::start(
        mem::take(&mut lobby),
        SeedableRng::from_entropy(),
    ));

    if let Some(game) = game.as_ref() {
        app.connections
            .lock()
            .unwrap()
            .broadcast_each(move |id| GameTemplate::render_with(game, Some(id)));
        return StatusCode::OK.into_response();
    }
    unreachable!()
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
        GameTemplate::render_with(game, id)
    } else {
        Err(ErrorResponse::from(Redirect::to("/lobby")))
    }
}

pub async fn get_game_actions(
    app: State<AppState>,
    cookies: PrivateCookieJar,
) -> Result<Html<String>, ErrorResponse> {
    let cookie = cookies.get("player_id").ok_or("missing cookie")?;
    let mut game = app.game.lock().unwrap();
    let game = game.as_mut().ok_or("game hasn't started")?;

    let active_player = game.active_player()?;

    if cfg!(not(feature = "dev")) && cookie.value() != active_player.id {
        return Err((StatusCode::BAD_REQUEST, "not your turn!").into());
    }

    MenuTemplate::from(game, Some(cookie.value())).to_html()
}

pub async fn get_game_city(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    path: Path<PlayerName>,
) -> Result<Html<String>, ErrorResponse> {
    let cookie = cookies.get("player_id");
    let id = cookie.as_ref().map(|c| c.value());
    let game = app.game.lock().unwrap();
    if let Some(game) = game.as_ref() {
        let p = game
            .players
            .iter()
            .find(|p| p.name == path.0)
            .ok_or("no player with that name")?;
        CityRootTemplate::from(game, p.index, id).to_html()
    } else {
        Err(ErrorResponse::from(Redirect::to("/lobby")))
    }
}

pub async fn get_game_logs(
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

fn bad_request(err: Cow<'static, str>) -> ErrorResponse {
    (
        StatusCode::BAD_REQUEST,
        [("HX-Retarget", "#errors"), ("HX-Reswap", "innerHTML")],
        err,
    )
        .into()
}

async fn submit_game_action(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    action: axum::Json<ActionSubmission>,
) -> Result<Response> {
    let cookie = cookies.get("player_id").ok_or("missing cookie")?;
    let mut game = app.game.lock().unwrap();
    let game = game.as_mut().ok_or("game hasn't started")?;

    let active_player = game.active_player()?;

    if cfg!(not(feature = "dev")) && cookie.value() != active_player.id {
        return Err(bad_request("not your turn!".into()));
    }

    log::info!("{:#?}", action.0);
    match action.0 {
        ActionSubmission::Complete(action) => {
            match game.perform(action) {
                Ok(()) => {
                    // TODO: broadcast other
                    let g = &game;
                    app.connections
                        .lock()
                        .unwrap()
                        .broadcast_each(move |id| GameTemplate::render_with(g, Some(id)));

                    Ok(StatusCode::OK.into_response())
                }
                Err(error) => Err(bad_request(error)),
            }
        }
        ActionSubmission::Incomplete { action } => match action {
            ActionTag::Assassinate => {
                let rendered = SelectRoleMenu {
                    context: GameContext::from_game(game),
                    roles: game
                        .characters
                        .iter_c()
                        .filter(|c| c.role.rank() > Rank::One)
                        .map(|c| RoleTemplate::from(c.role, 150.0))
                        .collect(),
                    header: "Select a role".into(),
                    action: ActionTag::Assassinate,
                }
                .to_html()?;
                Ok(rendered.into_response())
            }
            ActionTag::Steal => {
                let rendered = SelectRoleMenu {
                    context: GameContext::from_game(game),
                    roles: game
                        .characters
                        .iter_c()
                        .filter(|c| {
                            c.role.rank() > Rank::Two
                                && c.markers.iter().all(|m| *m != Marker::Killed)
                        })
                        .map(|c| RoleTemplate::from(c.role, 150.0))
                        .collect(),
                    header: "Select a role".into(),
                    action: ActionTag::Steal,
                }
                .to_html()?;
                Ok(rendered.into_response())
            }
            ActionTag::Magic => {
                let rendered = MagicMenu {}.to_html()?;
                Ok(rendered.into_response())
            }
            ActionTag::Build => {
                let rendered = BuildMenu {}.to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::WarlordDestroy => {
                let rendered = WarlordMenu::from_game(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Beautify => {
                let rendered = BeautifyMenu {}.to_html()?;
                Ok(rendered.into_response())
            }

            _ => Err(bad_request("missing selection".into())),
        },
    }
}

async fn get_game_menu(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    path: Path<String>,
) -> Result<Response> {
    let cookie = cookies.get("player_id").ok_or("missing cookie")?;
    let mut game = app.game.lock().unwrap();
    let game = game.as_mut().ok_or("game hasn't started")?;

    let active_player = game.active_player()?;

    if cfg!(not(feature = "dev")) && cookie.value() != active_player.id {
        return Err((StatusCode::BAD_REQUEST, "not your turn!").into());
    }

    match path.0.borrow() {
        "magic-swap-deck" => {
            let rendered = MagicSwapDeckMenu {}.to_html()?;
            Ok(rendered.into_response())
        }
        "magic-swap-player" => {
            let rendered = MagicSwapPlayerMenu {
                players: game
                    .players
                    .iter()
                    .filter(|p| active_player.id != p.id)
                    .map(|p| p.name.0.borrow())
                    .collect::<Vec<_>>(),
            }
            .to_html()?;
            Ok(rendered.into_response())
        }
        _ => todo!(),
    }
}
