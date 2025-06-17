use crate::server::state::AppState;
use crate::templates::game::menu::*;
use crate::templates::game::menus::*;
use crate::templates::game::*;
use crate::templates::lobby::*;
use crate::templates::*;
use askama::Template;
use cookie::SameSite;
use poem::endpoint::StaticFilesEndpoint;
use poem::web::websocket::WebSocket;
use poem::Endpoint;
use poem::{
    error::ParseQueryError,
    get, handler,
    http::StatusCode,
    http::{Method, Uri},
    middleware::AddData,
    middleware::Tracing,
    post,
    session::{CookieConfig, CookieSession, Session},
    web::{Data, Html, Json, Path, Query, Redirect},
    EndpointExt, IntoResponse, Response, Result, Route,
};

use citadels::actions::{ActionSubmission, ActionTag};
use citadels::districts::DistrictName;
use citadels::game::Game;
use citadels::lobby::ConfigOption;
use citadels::random::seed_from_entropy;
use citadels::roles::RoleName;
use citadels::types::{Marker, PlayerName};
use serde::Deserialize;
use sqlx::{Pool, Postgres};
use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
use time::Duration;
use uuid::Uuid;

fn form_feedback(err: Cow<'static, str>) -> Response {
    Response::default()
        .with_status(StatusCode::BAD_REQUEST)
        .with_header("HX-Retarget", "#error")
        .with_header("HX-Reswap", "innerHTML")
        .with_body(err.to_string())
        .into_response()
}

async fn log_action(
    db: &Pool<Postgres>,
    game_id: i32,
    action: serde_json::Value,
    player_name: Option<&PlayerName>,
) -> () {
    sqlx::query!(
        "insert into logs (game_id, action, player_name) values($1,$2, $3)",
        game_id,
        action,
        player_name.as_ref().map(|p| p.0.as_ref())
    )
    .execute(db)
    .await
    .unwrap();
}

pub struct GameRow {
    id: i32,
    state: Game,
}

pub fn get_router() -> impl Endpoint {
    let context = AppState::default();

    Route::new()
        .at("/", get(index))
        .at("/ws", get(get_ws))
        .at("/lobby", get(get_lobby))
        .at(
            "/lobby/config/districts",
            get(get_district_config).post(post_district_config),
        )
        .at(
            "/lobby/config/roles",
            get(get_role_config).post(post_role_config),
        )
        .at("/lobby/config/anarchy", post(post_anarchy))
        .at("/lobby/register", post(register))
        // .at("/game", get(game))
        // .at("/game/actions", get(get_game_actions))
        // .at("/game/city/:player_name", get(get_game_city))
        // .at("/game", post(start))
        // .at("/game/action", post(submit_game_action))
        // .at("/game/menu/:menu", get(get_game_menu))
        .nest("/public", StaticFilesEndpoint::new("public"))
        .with(AddData::new(context))
        .with(CookieSession::new(
            CookieConfig::default()
                .secure(true)
                .same_site(SameSite::Lax),
        ))
        .with(Tracing)
}

#[handler]
async fn index() -> Response {
    Redirect::temporary("/lobby").into_response()
}

#[handler]
async fn get_lobby(app: Data<&AppState>, session: &Session) -> Response {
    let player_id: Option<String> = session.get("player_id");

    if player_id.is_none() {
        log::info!("Setting new player_id cookie with 1 week expiry");
        let id = Uuid::new_v4().to_string();
        session.set("player_id", id);
    }

    let active_game = active_game(&app.db).await;

    if active_game.is_some() {
        return Redirect::temporary("/game").into_response();
    }

    let lobby = app.lobby.lock().await;

    Html(
        LobbyTemplate {
            players: &lobby.players,
            themes: &DAISY_THEMES,
        }
        .render()
        .unwrap(),
    )
    .into_response()
}

#[handler]
async fn get_district_config(app: Data<&AppState>) -> Response {
    let lobby = app.lobby.lock().await;
    DistrictConfigTemplate::from_config(lobby.config.districts.borrow())
        .to_html()
        .into_response()
}

#[handler]

async fn post_district_config(
    app: Data<&AppState>,
    form: Json<HashMap<DistrictName, ConfigOption>>,
) -> Response {
    let mut lobby = app.lobby.lock().await;
    lobby.config.districts = form.0;
    DistrictConfigTemplate::from_config(lobby.config.districts.borrow())
        .to_html()
        .into_response()
}

#[handler]
async fn get_role_config(app: Data<&AppState>) -> Response {
    let lobby = app.lobby.lock().await;
    RoleConfigTemplate::from_config(lobby.config.roles.borrow(), &HashSet::new())
        .to_html()
        .into_response()
}

#[handler]
async fn post_role_config(app: Data<&AppState>, form: Json<HashMap<RoleName, String>>) -> Response {
    let mut lobby = app.lobby.lock().await;
    log::info!("{:?}", form);

    let roles = form.0.into_keys().collect::<HashSet<_>>();
    if let Err((roles, ranks)) = lobby.config.set_roles(roles) {
        RoleConfigTemplate::from_config(&roles, &ranks)
            .to_html()
            .with_status(StatusCode::BAD_REQUEST)
            .into_response()
    } else {
        RoleConfigTemplate::from_config(&lobby.config.roles, &HashSet::new())
            .to_html()
            .into_response()
    }
}

#[handler]
async fn post_anarchy(app: Data<&AppState>, checked: Json<HashMap<String, String>>) -> Response {
    let mut lobby = app.lobby.lock().await;
    lobby.config.role_anarchy = checked.get("role_anarchy").is_some();
    log::info!("Set role_anarchy: {}", lobby.config.role_anarchy);
    StatusCode::OK.into_response()
}

#[derive(Deserialize)]
pub struct Register {
    username: String,
}

#[handler]
async fn register(app: Data<&AppState>, session: &Session, form: Json<Register>) -> Response {
    let username = form.username.trim();
    if username.len() == 0 {
        return form_feedback("username cannot be empty".into());
    }
    if username.chars().any(|c| !c.is_ascii_alphanumeric()) {
        return form_feedback("username can only contain letter a-z, A-Z, or digits".into());
    }
    const MAX_LEN: usize = 20;
    if username.len() > MAX_LEN {
        return form_feedback(
            format!("username cannot be more than {} characters long.", MAX_LEN).into(),
        );
    }

    let player_id: String = session.get("player_id").unwrap();
    let mut lobby = app.lobby.lock().await;
    if let Err(err) = lobby.register(&player_id, username) {
        return form_feedback(err);
    }

    let html = Html(
        LobbyPlayersTemplate {
            players: &lobby.players,
        }
        .render()
        .unwrap(),
    );
    app.connections.lock().await.broadcast(html);

    StatusCode::OK.into_response()
}

#[handler]
async fn start(app: Data<&AppState>) -> Response {
    let lobby = app.lobby.lock().await;
    if lobby.players.len() < 2 {
        return form_feedback("Need at least 2 players to start a game".into());
    }

    if lobby.players.len() > 8 {
        return form_feedback("You cannot have more than 8 players per game".into());
    }

    let active_game =
        sqlx::query!("select 1 as exists from rooms join games on rooms.game_id = games.id")
            .fetch_optional(&app.db)
            .await
            .unwrap()
            .is_some();

    if active_game {
        return form_feedback("Can not overwrite a game in progress".into());
    }
    let seed = seed_from_entropy();
    match citadels::game::Game::start(lobby.clone(), seed) {
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
            let mut game_start_action = serde_json::to_value(lobby.clone()).unwrap();
            let json = game_start_action.as_object_mut().unwrap();
            json.insert("seed".to_string(), serde_json::to_value(seed).unwrap());
            json.insert(
                "action".to_string(),
                serde_json::Value::String("Start".to_string()),
            );

            log_action(&app.db, row.id, game_start_action, None).await;

            app.connections
                .lock()
                .await
                .broadcast_each(move |id| GameTemplate::render_with(&game, Some(id)));
        }
        Err(err) => {
            return form_feedback(err);
        }
    }

    return StatusCode::OK.into_response();
}

async fn active_game(db: &Pool<Postgres>) -> Option<GameRow> {
    let row = sqlx::query!(
        "select games.id, games.state from rooms join games on rooms.game_id = games.id
where rooms.id = 1
order by rooms.id "
    )
    .fetch_optional(db)
    .await
    .unwrap();
    row.map(|g| GameRow {
        id: g.id,
        state: serde_json::from_value(g.state).unwrap(),
    })
}
//
// #[handler]
// async fn game(app: Data<&AppState>, session: &Session) -> Result<Html<String>, ErrorResponse> {
//     let cookie = session.get("player_id");
//     let id = cookie.as_ref().map(|c| c.value());
//     let game = active_game(&app.db).await;
//     let game = game.as_ref();
//     if let Some(game) = game.as_ref() {
//         GameTemplate::render_with(&game.state, id)
//     } else {
//         Err(ErrorResponse::from(Redirect::to("/lobby")))
//     }
// }
//
// #[handler]
// async fn get_game_actions(app: Data<&AppState>, session: &Session) -> Response {
//     let cookie = session.get("player_id");
//     let game = active_game(&app.db).await;
//     let game = game.as_mut().ok_or("game hasn't started")?;
//
//     MenuTemplate::from(&game.state, cookie.as_ref().map(|c| c.value())).to_html()
// }
//
// #[handler]
// async fn get_game_city(
//     app: Data<&AppState>,
//     session: &Session,
//     path: Path<PlayerName>,
// ) -> Result<Html<String>, ErrorResponse> {
//     let cookie = session.get("player_id");
//     let id = cookie.as_ref().map(|c| c.value());
//     let game = active_game(&app.db).await;
//     if let Some(game) = game.as_ref() {
//         let p = game
//             .state
//             .players
//             .iter()
//             .find(|p| p.name == path.0)
//             .ok_or("no player with that name")?;
//         CityRootTemplate::from(&game.state, p.index, id).to_html()
//     } else {
//         Err(ErrorResponse::from(Redirect::to("/lobby")))
//     }
// }

#[handler]
async fn get_ws(state: Data<&AppState>, session: &Session, ws: WebSocket) -> Response {
    let player_id = session.get("player_id").expect("player_id");

    let conn = state.connections.clone();
    ws.on_upgrade(|socket| async move {
        let conn = &mut conn.lock().await;
        crate::server::ws::handle_socket(conn, player_id, socket).await;
    })
    .into_response()
}
//

//
// #[handler]
// async fn submit_game_action(
//     app: Data<&AppState>,
//     session: &Session,
//     action: axum::Json<ActionSubmission>,
// ) -> Result<Response> {
//     let cookie = session.get("player_id").ok_or("missing cookie")?;
//     let mut game = active_game(&app.db).await;
//     let game = game.as_mut().ok_or("game hasn't started")?;
//     log::info!("{:#?}", action.0);
//     let player_id = cookie.value();
//
//     match action.0 {
//         ActionSubmission::Complete(action) => {
//             let p = game
//                 .state
//                 .players
//                 .iter()
//                 .find(|p| p.id == player_id)
//                 .unwrap();
//             let action_json = serde_json::to_value(&action).unwrap();
//             log_action(&app.db, game.id, action_json, Some(&p.name)).await;
//
//             match game.state.perform(action, player_id) {
//                 Ok(()) => {
//                     //
//                     sqlx::query!(
//                         "update games set state = $1 where id = $2",
//                         serde_json::to_value(&game.state).unwrap(),
//                         game.id
//                     )
//                     .execute(&app.db)
//                     .await
//                     .unwrap();
//                     app.connections
//                         .lock()
//                         .await
//                         .broadcast_each(move |id| GameTemplate::render_with(&game.state, Some(id)));
//
//                     Ok((StatusCode::OK, [("HX-Reswap", "none")]).into_response())
//                 }
//                 Err(error) => Err(form_feedback(error)),
//             }
//         }
//         ActionSubmission::Incomplete { action } => match action {
//             ActionTag::Assassinate => {
//                 let rendered = SelectRoleMenu {
//                     roles: game
//                         .state
//                         .characters
//                         .0
//                         .iter()
//                         .filter(|c| !c.revealed)
//                         .map(|c| RoleTemplate::from(c.role, 150.0))
//                         .collect(),
//                     context: GameContext::from_game(&game.state, Some(cookie.value())),
//                     header: "Assassin".into(),
//                     action: ActionTag::Assassinate,
//                 }
//                 .to_html()?;
//                 Ok(rendered.into_response())
//             }
//             ActionTag::Steal => {
//                 let rendered = SelectRoleMenu {
//                     roles: game
//                         .state
//                         .characters
//                         .0
//                         .iter()
//                         .filter(|c| {
//                             !c.revealed
//                                 && c.markers
//                                     .iter()
//                                     .all(|m| *m != Marker::Killed && *m != Marker::Bewitched)
//                         })
//                         .map(|c| RoleTemplate::from(c.role, 150.0))
//                         .collect(),
//                     context: GameContext::from_game(&game.state, Some(cookie.value())),
//                     header: "Thief".into(),
//                     action: ActionTag::Steal,
//                 }
//                 .to_html()?;
//                 Ok(rendered.into_response())
//             }
//             ActionTag::Magic => {
//                 let rendered = MagicMenu {}.to_html()?;
//                 Ok(rendered.into_response())
//             }
//             ActionTag::Build => {
//                 let rendered = BuildMenu::from_game(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::WarlordDestroy => {
//                 let rendered = WarlordMenu::from_game(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::Beautify => {
//                 let rendered = BeautifyMenu {}.to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::SendWarrants => {
//                 let rendered = SendWarrantsMenu::from_game(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::Blackmail => {
//                 let rendered = BlackmailMenu::from_game(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::NavigatorGain => {
//                 let rendered = NavigatorMenu {}.to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::Museum => {
//                 let rendered = MuseumMenu {}.to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::ResourcesFromReligion => {
//                 let rendered = AbbotCollectResourcesMenu::from(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::TakeFromRich => {
//                 let rendered = AbbotTakeFromRichMenu::from(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::Spy => {
//                 let rendered = SpyMenu::from(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::Armory => {
//                 let rendered = ArmoryMenu::from_game(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::MarshalSeize => {
//                 let rendered = MarshalMenu::from_game(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::DiplomatTrade => {
//                 let rendered = DiplomatMenu::from_game(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::Laboratory => {
//                 let rendered = LaboratoryMenu {}.to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::EmperorGiveCrown => {
//                 let rendered = EmperorMenu::from_game(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             ActionTag::WizardPeek => {
//                 let rendered = WizardMenu::from_game(&game.state).to_html()?;
//                 Ok(rendered.into_response())
//             }
//
//             _ => Err(form_feedback("missing selection".into())),
//         },
//     }
// }
// //
// // #[handler]
// // async fn get_game_menu(
// //     app: Data<&AppState>,
// //     session: &Session,
// //     path: Path<String>,
// // ) -> Result<Response> {
// //     let player_id: String = session.get("player_id").ok_or("missing cookie")?;
// //     let mut game = active_game(&app.db).await;
// //     let game = game.as_mut().ok_or("game hasn't started")?;
// //
// //     let active_player = game.state.active_player()?;
// //
// //     if cookie.value() != active_player.id {
// //         return Err((StatusCode::BAD_REQUEST, "not your turn!").into());
// //     }
// //
// //     match path.0.borrow() {
// //         "cardinal" => {
// //             let rendered = CardinalMenu {
// //                 players: game
// //                     .state
// //                     .players
// //                     .iter()
// //                     .filter(|p| active_player.id != p.id)
// //                     .map(|p| p.name.borrow())
// //                     .collect(),
// //                 hand: active_player
// //                     .hand
// //                     .iter()
// //                     .map(|d| DistrictTemplate::from(*d))
// //                     .collect(),
// //             }
// //             .to_html()?;
// //             Ok(rendered.into_response())
// //         }
// //
// //         "necropolis" => {
// //             let rendered = NecropolisMenu {
// //                 city: CityTemplate::from(&game.state, active_player.index, None),
// //             }
// //             .to_html()?;
// //             Ok(rendered.into_response())
// //         }
// //
// //         "thieves_den" => {
// //             let rendered = ThievesDenMenu {
// //                 hand: active_player
// //                     .hand
// //                     .iter()
// //                     .map(|d| DistrictTemplate::from(*d))
// //                     .collect(),
// //             }
// //             .to_html()?;
// //             Ok(rendered.into_response())
// //         }
// //         "magic-swap-deck" => {
// //             let rendered = MagicSwapDeckMenu {}.to_html()?;
// //             Ok(rendered.into_response())
// //         }
// //         "magic-swap-player" => {
// //             let rendered = MagicSwapPlayerMenu {
// //                 players: game
// //                     .state
// //                     .players
// //                     .iter()
// //                     .filter(|p| active_player.id != p.id)
// //                     .map(|p| p.name.0.borrow())
// //                     .collect::<Vec<_>>(),
// //             }
// //             .to_html()?;
// //             Ok(rendered.into_response())
// //         }
// //         _ => Ok(StatusCode::NOT_FOUND.into_response()),
// //     }
// // }
// //
