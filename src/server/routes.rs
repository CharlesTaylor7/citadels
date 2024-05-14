use crate::actions::{ActionSubmission, ActionTag};
use crate::districts::DistrictName;
use crate::game::Game;
use crate::lobby::{ConfigOption, Lobby};
use crate::roles::{Rank, RoleName};
use crate::server::state::AppState;
use crate::strings::{UserId, UserName};
use crate::templates::game::menu::*;
use crate::templates::game::menus::*;
use crate::templates::game::*;
use crate::templates::lobby::*;
use crate::types::Marker;
use crate::{markup, templates::*};

use axum::extract::{Json, Path, State};
use axum::response::{ErrorResponse, Html, Redirect, Response, Result};
use axum::routing::{get, post};
use axum::Router;
use axum::{extract::ws::WebSocketUpgrade, response::IntoResponse};
use axum_extra::extract::PrivateCookieJar;
use http::StatusCode;
use rand_core::SeedableRng;
use serde::Deserialize;
use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
use tower_http::services::ServeDir;
use tower_http::trace::TraceLayer;

pub fn get_router(state: AppState) -> Router {
    Router::new()
        .route("/", get(get_index))
        .route("/version", get(get_version))
        .route("/signup", get(get_signup))
        .route("/login", get(get_login))
        .route("/logout", post(post_logout))
        .route("/lobby", get(get_lobby))
        .route("/lobby/config/districts", get(get_district_config))
        .route("/lobby/config/districts", post(post_district_config))
        .route("/lobby/config/roles", get(get_role_config))
        .route("/lobby/config/roles", post(post_role_config))
        .route("/lobby/register", post(register))
        .route("/ws", get(get_ws))
        .route("/game", get(get_game))
        .route("/game/actions", get(get_game_actions))
        .route("/game/city/:player_name", get(get_game_city))
        .route("/game", post(start))
        .route("/game/action", post(submit_game_action))
        .route("/game/menu/:menu", get(get_game_menu))
        .nest_service("/public", ServeDir::new("public"))
        .layer(TraceLayer::new_for_http())
        .with_state(state)
}

async fn get_index() -> Result<Response, AnyhowError> {
    Ok((markup::index::page()).into_response())
}

async fn get_version() -> impl IntoResponse {
    std::env::var("GIT_SHA")
        .map_or(Cow::Borrowed("github.com/CharlesTaylor7/citadels"), |sha| {
            format!("github.com/CharlesTaylor7/citadels/commit/{sha}").into()
        })
        .into_response()
}

async fn get_signup(_app: State<AppState>, _cookies: PrivateCookieJar) -> impl IntoResponse {
    "TODO".into_response()
}
async fn get_login(_app: State<AppState>, _cookies: PrivateCookieJar) -> impl IntoResponse {
    "TODO".into_response()
}

async fn post_logout(app: State<AppState>, cookies: PrivateCookieJar) -> AppResponse {
    let session = app.sessions.read().await.session_from_cookies(&cookies);
    if let Some(session) = session {
        app.supabase.logout(&session.access_token).await?;
        Ok(().into_response())
    } else {
        Ok((StatusCode::BAD_REQUEST, "not logged in").into_response())
    }
}

async fn get_lobby(_app: State<AppState>, _cookies: PrivateCookieJar) -> impl IntoResponse {
    return crate::markup::lobby::page();
    /*
    if app.game.lock().unwrap().is_some() {
        return (cookies, Redirect::to("/game")).into_response();
    }

    let lobby = app.lobby.lock().unwrap();

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
        */
}

async fn get_district_config(app: State<AppState>) -> impl IntoResponse {
    let lobby = app.lobby.lock().unwrap();
    DistrictConfigTemplate::from_config(lobby.config.districts.borrow())
        .to_html()
        .into_response()
}

async fn post_district_config(
    app: State<AppState>,
    form: Json<HashMap<DistrictName, ConfigOption>>,
) -> impl IntoResponse {
    let mut lobby = app.lobby.lock().unwrap();
    lobby.config.districts = form.0;
    DistrictConfigTemplate::from_config(lobby.config.districts.borrow())
        .to_html()
        .into_response()
}

async fn get_role_config(app: State<AppState>) -> impl IntoResponse {
    let lobby = app.lobby.lock().unwrap();
    RoleConfigTemplate::from_config(lobby.config.roles.borrow(), &HashSet::new())
        .to_html()
        .into_response()
}

async fn post_role_config(
    app: State<AppState>,
    form: Json<HashMap<RoleName, String>>,
) -> Result<Response, ErrorResponse> {
    let mut lobby = app.lobby.lock().unwrap();
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

#[derive(Deserialize)]
pub struct Register {
    username: String,
}

async fn register(
    _app: State<AppState>,
    _cookies: PrivateCookieJar,
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
    // set profile's username in the db.
    Ok("TODO".into_response())
    /*
    let cookie = cookies.get("player_id").unwrap();
    let player_id = cookie.value();
    let mut lobby = app.lobby.lock().unwrap();
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
    app.connections.lock().unwrap().broadcast(html);

    Ok(StatusCode::OK.into_response())
    */
}

async fn start(app: State<AppState>) -> Result<Response> {
    let mut lobby = app.lobby.lock().unwrap();
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

    let mut game = app.game.lock().unwrap();
    if game.is_some() {
        return Err(form_feedback("Can not overwrite a game in progress".into()));
    }
    let clone = lobby.clone();
    match Game::start(clone, SeedableRng::from_entropy()) {
        Ok(ok) => {
            // Start the game, and remove all players from the lobby
            *game = Some(ok);
            *lobby = Lobby::default();
        }
        Err(err) => {
            return Err(form_feedback(err));
        }
    }

    if let Some(game) = game.as_ref() {
        app.connections
            .lock()
            .unwrap()
            .broadcast_each(move |id| GameTemplate::render_with(game, Some(id)));
        return Ok((StatusCode::OK).into_response());
    }
    unreachable!()
}

async fn get_game(_app: State<AppState>, _cookies: PrivateCookieJar) -> impl IntoResponse {
    return crate::markup::game::page();
    /*
    let cookie = cookies.get("player_id");
    let id = cookie.as_ref().map(|c| c.value());
    let game = app.game.lock().unwrap();
    let game = game.as_ref();
    if let Some(game) = game.as_ref() {
        GameTemplate::render_with(game, id)
    } else {
        Err(ErrorResponse::from(Redirect::to("/lobby")))
    }
    */
}

async fn get_game_actions(
    app: State<AppState>,
    cookies: PrivateCookieJar,
) -> Result<Html<String>, ErrorResponse> {
    let user_id = app.session(&cookies).await.map(|s| s.user_id);
    let mut game = app.game.lock().unwrap();
    let game = game.as_mut().ok_or("game hasn't started")?;

    MenuTemplate::from(game, user_id).to_html()
}

async fn get_game_city(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    path: Path<UserName>,
) -> Result<Html<String>, ErrorResponse> {
    let session = app.session(&cookies).await;
    let game = app.game.lock().unwrap();
    if let Some(game) = game.as_ref() {
        let p = game
            .players
            .iter()
            .find(|p| p.name == path.0)
            .ok_or("no player with that name")?;
        CityRootTemplate::from(game, p.index, session.map(|s| s.user_id)).to_html()
    } else {
        Err(ErrorResponse::from(Redirect::to("/lobby")))
    }
}

async fn get_ws(
    state: State<AppState>,
    cookies: PrivateCookieJar,
    ws: WebSocketUpgrade,
) -> impl IntoResponse {
    if let Some(session) = state.session(&cookies).await {
        ws.on_upgrade(move |socket| {
            crate::server::ws::handle_socket(state, session.user_id, socket)
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

async fn submit_game_action(
    app: State<AppState>,
    cookies: PrivateCookieJar,
    action: axum::Json<ActionSubmission>,
) -> Result<Response, ErrorResponse> {
    let session = if let Some(session) = app.session(&cookies).await {
        session
    } else {
        Err(AnyhowError(anyhow::anyhow!("not logged").into()).into_response())?
    };
    let user_id = session.user_id;
    let mut game = app.game.lock().unwrap();
    let game = game.as_mut().ok_or("game hasn't started")?;
    log::info!("{:#?}", action.0);
    match action.0 {
        ActionSubmission::Complete(action) => {
            match game.perform(action, user_id) {
                Ok(()) => {
                    // TODO: broadcast other
                    let g = &game;
                    app.connections
                        .lock()
                        .unwrap()
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
                        .iter_c()
                        .filter(|c| c.role.rank() > Rank::One)
                        .map(|c| RoleTemplate::from(c.role, 150.0))
                        .collect(),
                    context: GameContext::from_game(game, Some(user_id)),
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
                        .iter_c()
                        .filter(|c| {
                            c.role.rank() > Rank::Two
                                && c.markers
                                    .iter()
                                    .all(|m| *m != Marker::Killed && *m != Marker::Bewitched)
                        })
                        .map(|c| RoleTemplate::from(c.role, 150.0))
                        .collect(),
                    context: GameContext::from_game(game, Some(user_id)),
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
    let session = app.session(&cookies).await.ok_or("missing cookie")?;
    let mut game = app.game.lock().unwrap();
    let game = game.as_mut().ok_or("game hasn't started")?;

    let active_player = game.active_player()?;

    if session.user_id != active_player.id {
        return Err((StatusCode::BAD_REQUEST, "not your turn!").into());
    }

    match path.0.borrow() {
        "cardinal" => {
            let rendered = CardinalMenu {
                players: game
                    .players
                    .iter()
                    .filter(|p| active_player.id != p.id)
                    .map(|p| p.name.as_str())
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
                    .map(|p| p.name.as_str())
                    .collect::<Vec<_>>(),
            }
            .to_html()?;
            Ok(rendered.into_response())
        }
        _ => Ok(StatusCode::NOT_FOUND.into_response()),
    }
}

type AppResponse = Result<Response, AnyhowError>;

// Make our own error that wraps `anyhow::Error`.
struct AnyhowError(anyhow::Error);

// Tell axum how to convert `AppError` into a response.
impl IntoResponse for AnyhowError {
    fn into_response(self) -> Response {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!(
                "Internal Server Error\n{}",
                if cfg!(feature = "dev") {
                    self.0
                } else {
                    anyhow::anyhow!("")
                }
            ),
        )
            .into_response()
    }
}

// This enables using `?` on functions that return `Result<_, anyhow::Error>` to turn them into
// `Result<_, AppError>`. That way you don't need to do that manually.
impl<E> From<E> for AnyhowError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        Self(err.into())
    }
}
