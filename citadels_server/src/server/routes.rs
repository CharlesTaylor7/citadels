use crate::server::state::AppState;
use crate::templates::game::menu::*;
use crate::templates::game::menus::*;
use crate::templates::game::*;
use crate::templates::lobby::*;
use crate::templates::*;
use askama::Template;
use axum::debug_handler;
use axum::extract::{Json, Path, State};
use axum::response::{ErrorResponse, Html, Redirect, Response, Result};
use axum::routing::{get, post};
use axum::Router;
use axum::{extract::ws::WebSocketUpgrade, response::IntoResponse};
use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
use citadels::actions::{ActionSubmission, ActionTag};
use citadels::districts::DistrictName;
use citadels::game::Game;
use citadels::lobby::{ConfigOption, Lobby};
use citadels::random::seed_from_entropy;
use citadels::roles::RoleName;
use citadels::types::{Marker, PlayerName};
use http::StatusCode;
use rand_core::SeedableRng;
use serde::Deserialize;
use sqlx::Pool;
use sqlx::Postgres;
use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
use time::Duration;
use tower_http::services::ServeDir;
use uuid::Uuid;

pub fn get_router() -> Router {
    let context = AppState::default();

    Router::new()
        .route("/", get(index))
        .route("/version", get(get_version))
        .route("/lobby", get(get_lobby))
        .route("/lobby/config/districts", get(get_district_config))
        .route("/lobby/config/districts", post(post_district_config))
        .route("/lobby/config/roles", get(get_role_config))
        .route("/lobby/config/roles", post(post_role_config))
        .route("/lobby/config/anarchy", post(post_anarchy))
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

pub async fn get_version() -> impl IntoResponse {
    std::env::var("VERSION")
        .map_or(Cow::Borrowed("dev"), Cow::Owned)
        .into_response()
}

pub async fn get_lobby(app: State<AppState>, mut cookies: PrivateCookieJar) -> impl IntoResponse {
    let player_id = cookies.get("player_id");

    if player_id.is_none() {
        log::info!("Setting new player_id cookie with 1 week expiry");
        let id = Uuid::new_v4().to_string();
        let cookie = Cookie::build(("player_id", id)).max_age(Duration::WEEK);
        cookies = cookies.add(cookie);
    }

    let active_game =
        sqlx::query!("select 1 as exists from rooms join games on rooms.game_id = games.id")
            .fetch_optional(&app.db)
            .await
            .unwrap()
            .is_some();

    if active_game {
        return (cookies, Redirect::to("/game")).into_response();
    }

    let lobby = app.lobby.lock().await;

    (
        cookies,
        Html(
            LobbyTemplate {
                players: &lobby.players,
                themes: &DAISY_THEMES,
            }
            .render()
            .unwrap(),
        ),
    )
        .into_response()
}

pub async fn get_district_config(app: State<AppState>) -> impl IntoResponse {
    let lobby = app.lobby.lock().await;
    DistrictConfigTemplate::from_config(lobby.config.districts.borrow())
        .to_html()
        .into_response()
}

pub async fn post_district_config(
    app: State<AppState>,
    form: Json<HashMap<DistrictName, ConfigOption>>,
) -> impl IntoResponse {
    let mut lobby = app.lobby.lock().await;
    lobby.config.districts = form.0;
    DistrictConfigTemplate::from_config(lobby.config.districts.borrow())
        .to_html()
        .into_response()
}

pub async fn get_role_config(app: State<AppState>) -> impl IntoResponse {
    let lobby = app.lobby.lock().await;
    RoleConfigTemplate::from_config(lobby.config.roles.borrow(), &HashSet::new())
        .to_html()
        .into_response()
}

pub async fn post_role_config(
    app: State<AppState>,
    form: Json<HashMap<RoleName, String>>,
) -> Result<Response, ErrorResponse> {
    let mut lobby = app.lobby.lock().await;
    log::info!("{:?}", form);

    let roles = form.0.into_keys().collect::<HashSet<_>>();
    if let Err((roles, ranks)) = lobby.config.set_roles(roles) {
        Err((
            StatusCode::BAD_REQUEST,
            RoleConfigTemplate::from_config(&roles, &ranks).to_html(),
        )
            .into())
    } else {
        Ok((
            StatusCode::OK,
            RoleConfigTemplate::from_config(&lobby.config.roles, &HashSet::new()).to_html(),
        )
            .into_response())
    }
}

pub async fn post_anarchy(
    app: State<AppState>,
    checked: Json<HashMap<String, String>>,
) -> Result<Response, ErrorResponse> {
    let mut lobby = app.lobby.lock().await;
    lobby.config.role_anarchy = checked.get("role_anarchy").is_some();
    log::info!("Set role_anarchy: {}", lobby.config.role_anarchy);
    Ok(StatusCode::OK.into_response())
}

#[derive(Deserialize)]
pub struct Register {
    username: String,
}

pub async fn register(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    form: axum::Json<Register>,
) -> Result<Response> {
    let username = form.username.trim();
    if username.len() == 0 {
        return Err(form_feedback("username cannot be empty".into()));
    }
    if username.chars().any(|c| !c.is_ascii_alphanumeric()) {
        return Err(form_feedback(
            "username can only contain letter a-z, A-Z, or digits".into(),
        ));
    }
    const MAX_LEN: usize = 20;
    if username.len() > MAX_LEN {
        return Err(form_feedback(
            format!("username cannot be more than {} characters long.", MAX_LEN).into(),
        ));
    }

    let cookie = cookies.get("player_id").unwrap();
    let player_id = cookie.value();
    let mut lobby = app.lobby.lock().await;
    if let Err(err) = lobby.register(player_id, username) {
        return Err(form_feedback(err));
    }

    let html = Html(
        LobbyPlayersTemplate {
            players: &lobby.players,
        }
        .render()
        .unwrap(),
    );
    app.connections.lock().await.broadcast(html);

    Ok(StatusCode::OK.into_response())
}

#[debug_handler]
pub async fn start(app: State<AppState>) -> Result<Response> {
    let mut lobby = app.lobby.lock().await;
    if lobby.players.len() < 2 {
        return Err(form_feedback(
            "Need at least 2 players to start a game".into(),
        ));
    }

    if lobby.players.len() > 8 {
        return Err(form_feedback(
            "You cannot have more than 8 players per game".into(),
        ));
    }

    let active_game =
        sqlx::query!("select 1 as exists from rooms join games on rooms.game_id = games.id")
            .fetch_optional(&app.db)
            .await
            .unwrap()
            .is_some();

    if active_game {
        return Err(form_feedback("Can not overwrite a game in progress".into()));
    }
    let seed = seed_from_entropy();
    match Game::start(&app.db, lobby.clone(), seed).await {
        Ok(game) => {
            let row = sqlx::query!(
                "insert into games (state) values ($1) returning id",
                serde_json::to_value(&game).unwrap()
            )
            .fetch_one(&app.db)
            .await
            .unwrap();
            sqlx::query!("update rooms set game_id = $1 where rooms.id = 1", row.id)
                .execute(&app.db)
                .await
                .unwrap();
            app.connections
                .lock()
                .await
                .broadcast_each(move |id| GameTemplate::render_with(&game, Some(id)));
        }
        Err(err) => {
            return Err(form_feedback(err));
        }
    }

    return Ok((StatusCode::OK).into_response());
}

pub async fn active_game(db: &Pool<Postgres>) -> Option<Game> {
    let row = sqlx::query!(
        "select state from rooms join games on rooms.game_id = games.id order by rooms.id limit 1"
    )
    .fetch_optional(db)
    .await
    .unwrap();
    row.map(|g| serde_json::from_value(g.state).unwrap())
}

pub async fn game(
    app: State<AppState>,
    cookies: PrivateCookieJar,
) -> Result<Html<String>, ErrorResponse> {
    let cookie = cookies.get("player_id");
    let id = cookie.as_ref().map(|c| c.value());
    let game = active_game(&app.db).await;
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
    let cookie = cookies.get("player_id");
    let mut game = active_game(&app.db).await;
    let game = game.as_mut().ok_or("game hasn't started")?;

    MenuTemplate::from(game, cookie.as_ref().map(|c| c.value())).to_html()
}

pub async fn get_game_city(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    path: Path<PlayerName>,
) -> Result<Html<String>, ErrorResponse> {
    let cookie = cookies.get("player_id");
    let id = cookie.as_ref().map(|c| c.value());
    let game = active_game(&app.db).await;
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

fn form_feedback(err: Cow<'static, str>) -> ErrorResponse {
    (
        StatusCode::BAD_REQUEST,
        [("HX-Retarget", "#error"), ("HX-Reswap", "innerHTML")],
        err,
    )
        .into()
}

#[debug_handler]
async fn submit_game_action(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    action: axum::Json<ActionSubmission>,
) -> Result<Response> {
    let cookie = cookies.get("player_id").ok_or("missing cookie")?;
    let mut game = active_game(&app.db).await;
    let game = game.as_mut().ok_or("game hasn't started")?;
    log::info!("{:#?}", action.0);
    let player_id = cookie.value();

    match action.0 {
        ActionSubmission::Complete(action) => {
            match game.perform(&app.db, action.clone(), player_id).await {
                Ok(()) => {
                    //
                    let g = &game;
                    app.connections
                        .lock()
                        .await
                        .broadcast_each(move |id| GameTemplate::render_with(g, Some(id)));

                    Ok((StatusCode::OK, [("HX-Reswap", "none")]).into_response())
                }
                Err(error) => Err(form_feedback(error)),
            }
        }
        ActionSubmission::Incomplete { action } => match action {
            ActionTag::Assassinate => {
                let rendered = SelectRoleMenu {
                    roles: game
                        .characters
                        .0
                        .iter()
                        .filter(|c| !c.revealed)
                        .map(|c| RoleTemplate::from(c.role, 150.0))
                        .collect(),
                    context: GameContext::from_game(&game, Some(cookie.value())),
                    header: "Assassin".into(),
                    action: ActionTag::Assassinate,
                }
                .to_html()?;
                Ok(rendered.into_response())
            }
            ActionTag::Steal => {
                let rendered = SelectRoleMenu {
                    roles: game
                        .characters
                        .0
                        .iter()
                        .filter(|c| {
                            !c.revealed
                                && c.markers
                                    .iter()
                                    .all(|m| *m != Marker::Killed && *m != Marker::Bewitched)
                        })
                        .map(|c| RoleTemplate::from(c.role, 150.0))
                        .collect(),
                    context: GameContext::from_game(game, Some(cookie.value())),
                    header: "Thief".into(),
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
                let rendered = BuildMenu::from_game(game).to_html()?;
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

            ActionTag::SendWarrants => {
                let rendered = SendWarrantsMenu::from_game(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Blackmail => {
                let rendered = BlackmailMenu::from_game(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::NavigatorGain => {
                let rendered = NavigatorMenu {}.to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Museum => {
                let rendered = MuseumMenu {}.to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::ResourcesFromReligion => {
                let rendered = AbbotCollectResourcesMenu::from(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::TakeFromRich => {
                let rendered = AbbotTakeFromRichMenu::from(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Spy => {
                let rendered = SpyMenu::from(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Armory => {
                let rendered = ArmoryMenu::from_game(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::MarshalSeize => {
                let rendered = MarshalMenu::from_game(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::DiplomatTrade => {
                let rendered = DiplomatMenu::from_game(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Laboratory => {
                let rendered = LaboratoryMenu {}.to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::EmperorGiveCrown => {
                let rendered = EmperorMenu::from_game(game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::WizardPeek => {
                let rendered = WizardMenu::from_game(game).to_html()?;
                Ok(rendered.into_response())
            }

            _ => Err(form_feedback("missing selection".into())),
        },
    }
}

async fn get_game_menu(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    path: Path<String>,
) -> Result<Response> {
    let cookie = cookies.get("player_id").ok_or("missing cookie")?;
    let mut game = active_game(&app.db).await;
    let game = game.as_mut().ok_or("game hasn't started")?;

    let active_player = game.active_player()?;

    if cookie.value() != active_player.id {
        return Err((StatusCode::BAD_REQUEST, "not your turn!").into());
    }

    match path.0.borrow() {
        "cardinal" => {
            let rendered = CardinalMenu {
                players: game
                    .players
                    .iter()
                    .filter(|p| active_player.id != p.id)
                    .map(|p| p.name.borrow())
                    .collect(),
                hand: active_player
                    .hand
                    .iter()
                    .map(|d| DistrictTemplate::from(*d))
                    .collect(),
            }
            .to_html()?;
            Ok(rendered.into_response())
        }

        "necropolis" => {
            let rendered = NecropolisMenu {
                city: CityTemplate::from(game, active_player.index, None),
            }
            .to_html()?;
            Ok(rendered.into_response())
        }

        "thieves_den" => {
            let rendered = ThievesDenMenu {
                hand: active_player
                    .hand
                    .iter()
                    .map(|d| DistrictTemplate::from(*d))
                    .collect(),
            }
            .to_html()?;
            Ok(rendered.into_response())
        }
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
        _ => Ok(StatusCode::NOT_FOUND.into_response()),
    }
}
