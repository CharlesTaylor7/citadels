use crate::server::ws::WsHtmxHandles;
use crate::templates::game::menu::*;
use crate::templates::game::menus::*;
use crate::templates::game::*;
use crate::templates::lobby::*;
use crate::templates::*;
use askama::Template;
use citadels::game::PlayerIndex;
use citadels::lobby::Lobby;
use citadels::types::PlayerId;
use poem::web::websocket::WebSocket;
use poem::{
    get, handler,
    http::StatusCode,
    post,
    session::Session,
    web::{Data, Html, Json, Path, Redirect},
    IntoResponse, Response, Route,
};
use std::sync::Arc;
use tokio::sync::Mutex;

use citadels::actions::{ActionSubmission, ActionTag};
use citadels::districts::DistrictName;
use citadels::game::GameState;
use citadels::lobby::ConfigOption;
use citadels::random::seed_from_entropy;
use citadels::roles::RoleName;
use citadels::types::{Marker, PlayerName};
use serde::Deserialize;
use sqlx::{Pool, Postgres};
use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
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
    player_index: Option<PlayerIndex>,
) -> () {
    sqlx::query!(
        "insert into action_logs (game_id, action, player_index) values($1, $2, $3)",
        game_id,
        action,
        player_index.map(|o| o.0 as i32)
    )
    .execute(db)
    .await
    .unwrap();
}

pub struct GameRow {
    id: i32,
    state: GameState,
}

pub fn htmx_routes() -> Route {
    Route::new()
        .at("/", get(get_index))
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
        .at("/game", get(get_game).post(start_game))
        .at("/game/actions", get(get_game_actions))
        .at("/game/city/:player_name", get(get_game_city))
        .at("/game/action", post(submit_game_action))
        .at("/game/menu/:menu", get(get_game_menu))
}

pub type InMemoryLobby = Arc<Mutex<Lobby>>;
type DB = Pool<Postgres>;

#[handler]
async fn get_index() -> impl IntoResponse {
    Redirect::temporary("/lobby")
}

#[handler]
async fn get_lobby(lobby: Data<&InMemoryLobby>, db: Data<&DB>, session: &Session) -> Response {
    let player_id: Option<String> = session.get("player_id");

    if player_id.is_none() {
        log::info!("Setting new cookie player_id with 1 week expiry");
        let id = Uuid::new_v4().to_string();
        session.set("player_id", id);
    }

    let active_game = active_game(&db).await;

    if active_game.is_some() {
        return Redirect::temporary("/game").into_response();
    }

    let lobby = lobby.lock().await;

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
async fn get_district_config(lobby: Data<&InMemoryLobby>) -> Response {
    let lobby = lobby.lock().await;
    DistrictConfigTemplate::from_config(lobby.config.districts.borrow())
        .to_html()
        .into_response()
}

#[handler]
async fn post_district_config(
    lobby: Data<&InMemoryLobby>,
    form: Json<HashMap<DistrictName, ConfigOption>>,
) -> Response {
    let mut lobby = lobby.lock().await;
    lobby.config.districts = form.0;
    DistrictConfigTemplate::from_config(lobby.config.districts.borrow())
        .to_html()
        .into_response()
}

#[handler]
async fn get_role_config(lobby: Data<&InMemoryLobby>) -> Response {
    let lobby = lobby.lock().await;
    RoleConfigTemplate::from_config(lobby.config.roles.borrow(), &HashSet::new())
        .to_html()
        .into_response()
}

#[handler]
async fn post_role_config(
    lobby: Data<&InMemoryLobby>,
    form: Json<HashMap<RoleName, String>>,
) -> Response {
    let mut lobby = lobby.lock().await;
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
async fn post_anarchy(
    lobby: Data<&InMemoryLobby>,
    checked: Json<HashMap<String, String>>,
) -> Response {
    let mut lobby = lobby.lock().await;
    lobby.config.role_anarchy = checked.get("role_anarchy").is_some();
    log::info!("Set role_anarchy: {}", lobby.config.role_anarchy);
    StatusCode::OK.into_response()
}

#[derive(Deserialize)]
pub struct Register {
    username: String,
}

#[handler]
async fn register(
    connections: Data<&WsHtmxHandles>,
    lobby: Data<&InMemoryLobby>,
    session: &Session,
    form: Json<Register>,
) -> Response {
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
    let mut lobby = lobby.lock().await;
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
    connections.broadcast(html).await;

    StatusCode::OK.into_response()
}

#[handler]
async fn start_game(
    db: Data<&DB>,
    lobby: Data<&InMemoryLobby>,
    connections: Data<&WsHtmxHandles>,
) -> Response {
    let lobby = lobby.lock().await;
    if lobby.players.len() < 2 {
        return form_feedback("Need at least 2 players to start a game".into());
    }

    if lobby.players.len() > 8 {
        return form_feedback("You cannot have more than 8 players per game".into());
    }

    let active_game =
        sqlx::query!("select 1 as exists from rooms join games on rooms.game_id = games.id")
            .fetch_optional(db.0)
            .await
            .unwrap()
            .is_some();

    if active_game {
        return form_feedback("Can not overwrite a game in progress".into());
    }
    let seed = seed_from_entropy();
    match citadels::game::GameState::start(lobby.clone(), seed) {
        Ok(game) => {
            let row = sqlx::query!(
                "insert into games (state) values ($1) returning id",
                serde_json::to_value(&game).unwrap()
            )
            .fetch_one(db.0)
            .await
            .unwrap();
            sqlx::query!("update rooms set game_id = $1 where rooms.id = 1", row.id)
                .execute(db.0)
                .await
                .unwrap();
            let mut game_start_action = serde_json::to_value(lobby.clone()).unwrap();
            let json = game_start_action.as_object_mut().unwrap();
            json.insert("seed".to_string(), serde_json::to_value(seed).unwrap());
            json.insert(
                "action".to_string(),
                serde_json::Value::String("Start".to_string()),
            );

            log_action(db.0, row.id, game_start_action, None).await;

            connections
                .broadcast_each(move |id| GameTemplate::render_with(&game, Some(id.to_string())))
                .await;

            StatusCode::OK.into_response()
        }
        Err(err) => form_feedback(err),
    }
}

async fn active_game(db: &Pool<Postgres>) -> Option<GameRow> {
    let row = sqlx::query!(
        r#"select games.id, games.state from games where active = true order by id desc
-- from rooms join games on rooms.game_id = games.id
-- where rooms.id = 1
-- order by rooms.id 
"#
    )
    .fetch_optional(db)
    .await
    .unwrap();
    row.map(|g| GameRow {
        id: g.id,
        state: serde_json::from_value(g.state).unwrap(),
    })
}

#[handler]
async fn get_game(db: Data<&DB>, session: &Session) -> Response {
    let player_id: Option<String> = session.get("player_id");
    let game = active_game(&db).await;
    match game {
        Some(game) => GameTemplate::render_with(&game.state, player_id).into_response(),
        None => Redirect::temporary("/lobby").into_response(),
    }
}

#[handler]
async fn get_game_actions(db: Data<&DB>, session: &Session) -> Response {
    let player_id: Option<PlayerId> = session.get("player_id");
    let game = active_game(&db).await.unwrap();

    MenuTemplate::from(&game.state, player_id)
        .to_html()
        .into_response()
}

#[handler]
async fn get_game_city(db: Data<&DB>, session: &Session, path: Path<PlayerName>) -> Response {
    let player_id: Option<String> = session.get("player_id");
    let game: GameRow = active_game(&db).await.unwrap();
    let p = game
        .state
        .players
        .iter()
        .find(|p| p.name == path.0)
        .unwrap();
    CityRootTemplate::from(&game.state, p.index, player_id)
        .to_html()
        .into_response()
}

#[handler]
async fn get_ws(
    connections: Data<&WsHtmxHandles>,
    session: &Session,
    ws: WebSocket,
) -> impl IntoResponse {
    let player_id = session.get("player_id").expect("player_id");
    let connections = connections.clone();
    ws.on_upgrade(move |socket| crate::server::ws::handle_socket(connections, player_id, socket))
}

#[handler]
async fn submit_game_action(
    connections: Data<&WsHtmxHandles>,
    db: Data<&Pool<Postgres>>,
    session: &Session,
    action: Json<ActionSubmission>,
) -> Response {
    let player_id: String = session.get("player_id").unwrap();
    let mut game = active_game(&db).await.unwrap();
    log::info!("{:#?}", action.0);

    match action.0 {
        ActionSubmission::Complete(action) => {
            let p = game
                .state
                .players
                .iter()
                .find(|p| p.id == player_id)
                .unwrap();
            let action_json = serde_json::to_value(&action).unwrap();
            log_action(&db, game.id, action_json, Some(p.index)).await;

            match game.state.perform(action, &player_id) {
                Ok(()) => {
                    //
                    sqlx::query!(
                        "update games set state = $1 where id = $2",
                        serde_json::to_value(&game.state).unwrap(),
                        game.id
                    )
                    .execute(db.0)
                    .await
                    .unwrap();
                    connections
                        .broadcast_each(move |id| {
                            GameTemplate::render_with(&game.state, Some(id.to_string()))
                        })
                        .await;

                    Response::default()
                        .with_header("HX-Reswap", "none")
                        .into_response()
                }
                Err(error) => form_feedback(error),
            }
        }
        ActionSubmission::Incomplete { action } => match action {
            ActionTag::Assassinate => SelectRoleMenu {
                roles: game
                    .state
                    .characters
                    .0
                    .iter()
                    .filter(|c| !c.revealed)
                    .map(|c| RoleTemplate::from(c.role, 150.0))
                    .collect(),
                context: GameContext::from_game(&game.state, Some(player_id)),
                header: "Assassin".into(),
                action: ActionTag::Assassinate,
            }
            .to_html()
            .into_response(),
            ActionTag::Steal => SelectRoleMenu {
                roles: game
                    .state
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
                context: GameContext::from_game(&game.state, Some(player_id)),
                header: "Thief".into(),
                action: ActionTag::Steal,
            }
            .to_html()
            .into_response(),
            ActionTag::Magic => MagicMenu {}.to_html().into_response(),
            ActionTag::Build => BuildMenu::from_game(&game.state).to_html().into_response(),

            ActionTag::WarlordDestroy => WarlordMenu::from_game(&game.state)
                .to_html()
                .into_response(),

            ActionTag::Beautify => BeautifyMenu {}.to_html().into_response(),

            ActionTag::SendWarrants => SendWarrantsMenu::from_game(&game.state)
                .to_html()
                .into_response(),

            ActionTag::Blackmail => BlackmailMenu::from_game(&game.state)
                .to_html()
                .into_response(),

            ActionTag::NavigatorGain => NavigatorMenu {}.to_html().into_response(),

            ActionTag::Museum => MuseumMenu {}.to_html().into_response(),

            ActionTag::ResourcesFromReligion => AbbotCollectResourcesMenu::from(&game.state)
                .to_html()
                .into_response(),

            ActionTag::TakeFromRich => AbbotTakeFromRichMenu::from(&game.state)
                .to_html()
                .into_response(),

            ActionTag::Spy => SpyMenu::from(&game.state).to_html().into_response(),

            ActionTag::Armory => ArmoryMenu::from_game(&game.state).to_html().into_response(),

            ActionTag::MarshalSeize => MarshalMenu::from_game(&game.state)
                .to_html()
                .into_response(),

            ActionTag::DiplomatTrade => DiplomatMenu::from_game(&game.state)
                .to_html()
                .into_response(),

            ActionTag::Laboratory => LaboratoryMenu {}.to_html().into_response(),

            ActionTag::EmperorGiveCrown => EmperorMenu::from_game(&game.state)
                .to_html()
                .into_response(),

            ActionTag::WizardPeek => WizardMenu::from_game(&game.state).to_html().into_response(),
            _ => form_feedback("missing selection".into()),
        },
    }
}

#[handler]
async fn get_game_menu(db: Data<&DB>, session: &Session, path: Path<String>) -> Response {
    let player_id: String = session.get("player_id").unwrap();
    let mut game = active_game(&db).await.unwrap();

    let active_player = game.state.active_player().unwrap();

    if player_id != active_player.id {
        return StatusCode::FORBIDDEN.into_response();
    }

    match path.0.borrow() {
        "cardinal" => CardinalMenu {
            players: game
                .state
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
        .to_html()
        .into_response(),

        "necropolis" => NecropolisMenu {
            city: CityTemplate::from(&game.state, active_player.index, None),
        }
        .to_html()
        .into_response(),

        "thieves_den" => ThievesDenMenu {
            hand: active_player
                .hand
                .iter()
                .map(|d| DistrictTemplate::from(*d))
                .collect(),
        }
        .to_html()
        .into_response(),
        "magic-swap-deck" => MagicSwapDeckMenu {}.to_html().into_response(),
        "magic-swap-player" => MagicSwapPlayerMenu {
            players: game
                .state
                .players
                .iter()
                .filter(|p| active_player.id != p.id)
                .map(|p| p.name.0.borrow())
                .collect::<Vec<_>>(),
        }
        .to_html()
        .into_response(),
        _ => StatusCode::NOT_FOUND.into_response(),
    }
}
