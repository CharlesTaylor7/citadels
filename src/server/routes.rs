use super::errors::{AnyhowError, AnyhowResponse};
use super::models::CustomClaims;
use super::response;
use super::response::AppResponse;
use crate::actions::{ActionSubmission, ActionTag};
use crate::districts::DistrictName;
use crate::game::Game;
use crate::lobby::{ConfigOption, GameConfig};
use crate::roles::{Rank, RoleName};
use crate::server::state::AppState;
use crate::strings::UserName;
use crate::templates::game::menus::*;
use crate::templates::game::*;
use crate::templates::lobby::*;
use crate::types::Marker;
use crate::{markup, templates::*};
use axum::extract::{Json, Path, Query, State};
use axum::response::{ErrorResponse, Redirect, Response, Result};
use axum::routing::{get, post};
use axum::Router;
use axum::{extract::ws::WebSocketUpgrade, response::IntoResponse};
use http::{header, StatusCode};
use percent_encoding::utf8_percent_encode;
use serde::{Deserialize, Serialize};
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::env;
use tower_cookies::{CookieManagerLayer, Cookies};

pub fn get_router(state: AppState) -> Router {
    #[allow(unused_mut)]
    let mut router = Router::new()
        .route("/", get(get_index))
        .route("/version", get(get_version))
        .route("/login", get(get_login))
        .route("/oauth/signin", get(get_oauth_signin))
        .route("/oauth/callback", get(get_oauth_callback))
        .route("/oauth/logout", post(post_logout))
        .route("/profile", get(get_profile))
        .route("/profile", post(post_profile))
        .route("/lobby", get(get_lobby))
        .route("/lobby/create-room", post(create_room))
        .route("/lobby/join-room", post(join_room))
        .route("/lobby/leave-room", post(leave_room))
        .route("/lobby/kick-room", post(kick_room))
        .route("/lobby/start-game", post(start_game))
        .route("/game", get(get_game));
    //.route("/lobby/config/districts", get(get_district_config))
    //.route("/lobby/config/districts", post(post_district_config))
    //.route("/lobby/config/roles", get(get_role_config))
    //.route("/lobby/config/roles", post(post_role_config))
    //.route("/lobby/register", post(register))
    // .route("/ws", get(get_ws))
    // .route("/game/actions", get(get_game_actions))
    // .route("/game/city/:player_name", get(get_game_city))
    // .route("/game", post(start))
    // .route("/game/action", post(submit_game_action))
    // .route("/game/menu/:menu", get(get_game_menu))

    #[cfg(feature = "impersonate")]
    {
        router = router
            .route("/dev", get(dev::get_page))
            .route("/dev/create-profile", post(dev::post_create_profile))
            .route("/dev/impersonate", post(dev::post_impersonate))
    }

    #[cfg(feature = "dev")]
    {
        use tower_http::services::ServeDir;
        use tower_livereload::LiveReloadLayer;

        router = router.nest_service("/public", ServeDir::new("public"));
        //     .layer(LiveReloadLayer::new());
    }

    router.layer(CookieManagerLayer::new()).with_state(state)
}

#[cfg(feature = "impersonate")]
pub mod dev {
    use crate::{
        markup,
        server::{
            auth,
            response::{self, AppResponse},
            state::AppState,
        },
    };
    use axum::{extract::State, Json};
    use serde::Deserialize;
    use tower_cookies::Cookies;

    pub struct Profile {
        pub email: String,
        pub username: String,
    }
    pub async fn get_page(state: State<AppState>, cookies: Cookies) -> AppResponse {
        let mut transaction = state.service_transaction().await?;
        let profiles = sqlx::query_as!(
            Profile,
            r#"
            SELECT users.email AS "email!", COALESCE(profiles.username, '') AS "username!" 
            FROM auth.users AS users
            LEFT JOIN profiles ON profiles.user_id = users.id
            WHERE users.email IS NOT NULL
            "#,
        )
        .fetch_all(&mut *transaction)
        .await?;
        transaction.commit().await?;

        response::ok(markup::dev::page(&cookies, &profiles))
    }

    #[derive(Deserialize)]
    pub struct FakeProfile {
        username: String,
        email: String,
        impersonate: Option<String>,
    }

    pub async fn post_create_profile(
        state: State<AppState>,
        cookies: Cookies,
        body: Json<FakeProfile>,
    ) -> AppResponse {
        return Err(anyhow::anyhow!("TODO").into());
        /*
        let mut transaction = state.service_transaction().await?;
        let user = sqlx::query!(
            r#"
            select id::text AS "id!" from auth.users
            where email = $1
            "#,
            body.email
        )
        .fetch_optional(&mut *transaction)
        .await?;

        let user_id = if let Some(user) = user {
            user.id
        } else {
            let client = state.supabase.anon();
            let password = uuid::Uuid::new_v4().to_string();
            let signup = client
                .signup_email(EmailCreds {
                    email: &body.email,
                    password: &password,
                })
                .await?;
            signup.user.id.to_string()
        };
        if body.impersonate.is_some() {
            cookies.add(auth::cookie(
                "impersonate",
                user_id.clone(),
                time::Duration::days(1000),
            ));
        }

        let response = sqlx::query(
            r#"
            insert into profiles(user_id, username)
            values ($1::uuid, $2)
            on conflict (user_id) do update
                set username = $2
            "#,
        )
        .bind(user_id)
        .bind(&body.username)
        .execute(&mut *transaction)
        .await;
        log::info!("{:#?}", response);
        transaction.commit().await?;

        get_page(state, cookies).await
        */
    }

    #[derive(Deserialize)]
    pub struct Impersonate {
        user_id: String,
    }
    pub async fn post_impersonate(cookies: Cookies, body: Json<Impersonate>) -> AppResponse {
        cookies.add(auth::cookie(
            "impersonate",
            body.user_id.clone(),
            time::Duration::days(1000),
        ));

        response::ok(())
    }
}

async fn get_login(_app: State<AppState>, cookies: Cookies) -> impl IntoResponse {
    markup::login::page(&cookies)
}

async fn get_index(cookies: Cookies) -> Result<Response, AnyhowError> {
    Ok((markup::index::page(&cookies)).into_response())
}

async fn get_version() -> impl IntoResponse {
    let commit = std::env::var("GIT_SHA").map_or("main".into(), Cow::Owned);

    maud::html!(
        a href={"https://github.com/CharlesTaylor7/citadels/tree/" (commit)} {
            "Github"
        }
    )
}

async fn get_profile(state: State<AppState>, cookies: Cookies) -> AppResponse {
    /*
     let mut tx = state.user_transaction(&cookies).await?;
     let query = sqlx::query!("select username from profiles")
         .fetch_optional(&mut *tx)
         .await?;
     let username = if let Some(profile) = query {
         profile.username
     } else {
         let claims = state.user_claims(&cookies)?;
         match claims.user_metadata.custom_claims {
             Some(CustomClaims::DiscordClaims { global_name }) => global_name,
             None => "".to_owned(),
         }
     };
     //let user_id = todo!();
     //return markup::profile::page(None);
     //let profile = storage::profile(user_id).await;
    */
    let username = "".to_owned();
    response::ok(markup::profile::page(&cookies, username))
}

async fn post_profile(
    state: State<AppState>,
    cookies: Cookies,
    body: Json<Profile>,
) -> AppResponse {
    let mut transaction = state.user_transaction(&cookies).await?;
    let response = sqlx::query!(
        r#"
        insert into profiles(username)
        values ($1)
        on conflict (user_id) do update
            set username = $1
        "#,
        body.username
    )
    .execute(&mut *transaction)
    .await;
    log::info!("{:#?}", response);
    transaction.commit().await?;

    response::ok(())
}

async fn get_oauth_signin() -> impl IntoResponse {
    Redirect::to(&format!(
        "https://discord.com/api/v10/oauth2/authorize?prompt=login&client_id={}&response_type=code&redirect_uri={}&state={}&scope=identify%20email",
        env::var("DISCORD_CLIENT_ID").unwrap(),
        utf8_percent_encode(&callback_url(), percent_encoding::NON_ALPHANUMERIC),
        "123", // TODO: state
    ))
}

#[derive(Deserialize, Debug)]
pub struct DiscordSignIn {
    pub access_token: String, // Unlike Supabase, this is not a JWT
    pub refresh_token: String,
    pub expires_in: u32, // a week long
}

fn callback_url() -> String {
    format!("{}/oauth/callback", env::var("SITE_URL").unwrap(),)
}

async fn get_oauth_callback(
    app: State<AppState>,
    cookies: Cookies,
    query: Query<serde_json::Value>,
) -> AppResponse {
    let client = reqwest::Client::new();
    if let Some(code) = query.get("code").and_then(|code| code.as_str()) {
        let response = client
            .post("https://discord.com/api/v10/oauth2/token")
            .form(&[
                ("grant_type", "authorization_code"),
                ("code", code),
                ("redirect_uri", &callback_url()),
                ("scope", "identify email"),
                ("client_id", &env::var("DISCORD_CLIENT_ID").unwrap()),
                ("client_secret", &env::var("DISCORD_CLIENT_SECRET").unwrap()),
            ])
            .send()
            .await?;

        let body = response.bytes().await?;
        let json = serde_json::from_slice::<DiscordSignIn>(&body)?;

        let response = client
            .get("https://discord.com/api/v10/users/@me")
            .header("User-Agent", "DiscordBot (https://citadels.fly.dev, 0.7.1)")
            .header("Authorization", &format!("Bearer {}", json.access_token))
            .send()
            .await?;
        let body = response.bytes().await?;
        log::info!("@me: {:#?}", body);
    }

    // Workaround:
    // Chrome does not honor Set-Cookie headers when they are attached to 302 Redirects.
    // So instead we return a 200 Ok with a Refresh header.
    response::ok(())
    /*
    if let Some(verifier_cookie) = cookies.get("code_verifier") {
        let response = app
            .supabase
            .anon()
            .exchange_code_for_session(ExchangeOAuthCode {
                auth_code: &body.code,
                code_verifier: verifier_cookie.value(),
            })
            .await?;
        auth::remove_cookie(&cookies, "code_verifier");
        cookies.add(auth::cookie(
            "access_token",
            response.access_token,
            time::Duration::seconds(response.expires_in.into()),
        ));
        cookies.add(auth::cookie(
            "refresh_token",
            response.refresh_token,
            time::Duration::WEEK,
        ));

        let mut tx = app.user_transaction(&cookies).await?;
        let has_profile = sqlx::query!("select 1 as temp from profiles")
            .fetch_optional(&mut *tx)
            .await?
            .is_some();
        tx.commit().await?;

        let url = if has_profile { "/lobby" } else { "/profile" };

        response::ok([(header::REFRESH, format!("0;url={}", url))])
    } else {
        log::error!("no code verifier");
        response::ok(Redirect::to("/login"))
    }
    */
}

async fn post_logout(app: State<AppState>, cookies: Cookies) -> AppResponse {
    app.logout(cookies).await?;

    response::ok([(header::REFRESH, format!("0;url={}", "/"))])
}

#[derive(Debug)]
pub struct RoomSummary {
    pub id: uuid::Uuid,
    pub host: String,
    pub num_players: i64,
}
async fn get_lobby(app: State<AppState>, cookies: Cookies) -> AppResponse {
    let mut tx = app.user_transaction(&cookies).await?;

    let rooms = sqlx::query_as!(
        RoomSummary,
        r#"
        SELECT rooms.id, hosts.username AS host, COUNT(member.user_id) AS "num_players!" FROM rooms 
        JOIN profiles AS hosts ON hosts.user_id = rooms.host_id
        LEFT JOIN room_members AS member ON rooms.id = member.room_id
        GROUP BY (rooms.id, hosts.username)
        "#
    )
    .fetch_all(&mut *tx)
    .await?;

    tx.commit().await?;
    log::info!("rooms: {:#?}", rooms);
    response::ok(markup::lobby::page(&cookies, &rooms))
}

async fn create_room(app: State<AppState>, cookies: Cookies) -> AppResponse {
    let mut tx = app.user_transaction(&cookies).await?;
    let room = sqlx::query!(
        r#"
        INSERT INTO rooms (game_config) VALUES ($1) 
        RETURNING id 
        "#,
        serde_json::value::to_value(GameConfig::default())?,
    )
    .fetch_one(&mut *tx)
    .await?;
    sqlx::query!(
        r#"
        INSERT INTO room_members (room_id) VALUES ($1)
        "#,
        room.id,
    )
    .execute(&mut *tx)
    .await?;

    tx.commit().await?;
    response::ok(())
}

#[derive(Deserialize)]
pub struct JoinRoom {
    room_id: String,
}

async fn join_room(app: State<AppState>, cookies: Cookies, body: Json<JoinRoom>) -> AppResponse {
    let mut tx = app.user_transaction(&cookies).await?;
    sqlx::query(
        r#"
        INSERT INTO room_members (room_id) VALUES ($1::uuid)
        "#,
    )
    .bind(&body.room_id)
    .execute(&mut *tx)
    .await?;
    tx.commit().await?;
    response::ok(())
}

async fn leave_room(app: State<AppState>, cookies: Cookies) -> AppResponse {
    let mut tx = app.user_transaction(&cookies).await?;
    sqlx::query!(
        r#"
        DELETE FROM room_members WHERE room_members.user_id = current_user_id()
        "#,
    )
    .execute(&mut *tx)
    .await?;
    tx.commit().await?;
    response::ok(())
}

#[derive(Deserialize)]
pub struct KickRoom {
    user_id: String,
}
async fn kick_room(app: State<AppState>, cookies: Cookies, body: Json<KickRoom>) -> AppResponse {
    let mut tx = app.user_transaction(&cookies).await?;
    sqlx::query(
        r#"
        DELETE FROM room_members WHERE room_members.user_id = $1::uuid
        "#,
    )
    .bind(&body.user_id)
    .execute(&mut *tx)
    .await?;
    tx.commit().await?;
    response::ok(())
}

async fn start_game(app: State<AppState>, cookies: Cookies) -> AppResponse {
    let mut tx = app.user_transaction(&cookies).await?;
    tx.commit().await?;
    response::ok(())
}

async fn get_district_config(app: State<AppState>) -> impl IntoResponse {
    //let lobby = app.lobby.lock().unwrap();
    //
    DistrictConfigTemplate::from_config(&GameConfig::default().districts)
        .to_html()
        .into_response()
}

async fn post_district_config(
    app: State<AppState>,
    form: Json<HashMap<DistrictName, ConfigOption>>,
) -> impl IntoResponse {
    return "TODO";
    /*
    let mut lobby = app.lobby.lock().unwrap();
    lobby.config.districts = form.0;
    DistrictConfigTemplate::from_config(lobby.config.districts.borrow())
        .to_html()
        .into_response()
        */
}

async fn get_role_config(app: State<AppState>) -> impl IntoResponse {
    return "TODO";
    /*
    let lobby = app.lobby.lock().unwrap();
    RoleConfigTemplate::from_config(lobby.config.roles.borrow(), &HashSet::new())
        .to_html()
        .into_response()
        */
}

async fn post_role_config(
    app: State<AppState>,
    form: Json<HashMap<RoleName, String>>,
) -> AppResponse {
    return response::ok("TODO");
    /*
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
    */
}

async fn register(_app: State<AppState>, _cookies: Cookies) -> Result<Response> {
    Ok("TODO".into_response())
    /*
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
    */
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

async fn start(app: State<AppState>) -> AppResponse {
    response::ok("TODO")
    /*
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
        app.ws_connections
            .lock()
            .unwrap()
            .broadcast_each(move |id| GameTemplate::render_with(game, Some(id)));
        return Ok((StatusCode::OK).into_response());
    }
    unreachable!()
    */
}

async fn get_game(_app: State<AppState>, _cookies: Cookies) -> impl IntoResponse {
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

async fn get_game_actions(_app: State<AppState>, _cookies: Cookies) -> AnyhowResponse {
    Ok(().into_response())
    /*
    let user_id = app.session(&cookies).await.map(|s| s.user_id);
    let mut game = app.game.lock().unwrap();
    let game = game.as_mut().ok_or("game hasn't started")?;

    MenuTemplate::from(game, user_id).to_html()
    */
}

async fn get_game_city(
    _app: State<AppState>,
    _cookies: Cookies,
    _path: Path<UserName>,
) -> AnyhowResponse {
    Ok(().into_response())
    /*
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
    */
}

async fn get_ws(
    _state: State<AppState>,
    _cookies: Cookies,
    _ws: WebSocketUpgrade,
) -> impl IntoResponse {
    "TODO"
    /*
    if let Some(session) = state.session(&cookies).await {
        ws.on_upgrade(move |socket| {
            crate::server::ws::handle_socket(state, session.user_id, socket)
        })
        .into_response()
    } else {
        StatusCode::BAD_REQUEST.into_response()
    }
    */
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
    cookies: Cookies,
    action: axum::Json<ActionSubmission>,
) -> Result<Response, ErrorResponse> {
    let user_id = if let Ok(user_id) = app.user_id(&cookies).await {
        user_id
    } else {
        Err(AnyhowError(anyhow::anyhow!("not logged").into()).into_response())?
    };
    let mut game = Game::demo(3)?;
    log::info!("{:#?}", action.0);
    match action.0 {
        ActionSubmission::Complete(action) => {
            match game.perform(action, user_id) {
                Ok(()) => {
                    // TODO: broadcast other
                    let g = &game;
                    app.ws_connections
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
                    context: GameContext::from_game(&game, Some(user_id)),
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
                    context: GameContext::from_game(&game, Some(user_id)),
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
                let rendered = BuildMenu::from_game(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::WarlordDestroy => {
                let rendered = WarlordMenu::from_game(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Beautify => {
                let rendered = BeautifyMenu {}.to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::SendWarrants => {
                let rendered = SendWarrantsMenu::from_game(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Blackmail => {
                let rendered = BlackmailMenu::from_game(&game).to_html()?;
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
                let rendered = AbbotCollectResourcesMenu::from(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::TakeFromRich => {
                let rendered = AbbotTakeFromRichMenu::from(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Spy => {
                let rendered = SpyMenu::from(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Armory => {
                let rendered = ArmoryMenu::from_game(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::MarshalSeize => {
                let rendered = MarshalMenu::from_game(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::DiplomatTrade => {
                let rendered = DiplomatMenu::from_game(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::Laboratory => {
                let rendered = LaboratoryMenu {}.to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::EmperorGiveCrown => {
                let rendered = EmperorMenu::from_game(&game).to_html()?;
                Ok(rendered.into_response())
            }

            ActionTag::WizardPeek => {
                let rendered = WizardMenu::from_game(&game).to_html()?;
                Ok(rendered.into_response())
            }

            _ => Err(form_feedback("missing selection".into())),
        },
    }
}

async fn get_game_menu(
    app: State<AppState>,
    cookies: Cookies,
    path: Path<String>,
) -> Result<Response> {
    let user_id = app.user_id(&cookies).await.map_err(AnyhowError)?;
    let game = Game::demo(3)?;
    let active_player = game.active_player()?;

    if user_id != active_player.id {
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
                city: CityTemplate::from(&game, active_player.index, None),
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

/* DTOs */

#[derive(Deserialize)]
struct OAuthProvider {
    provider: String,
}

#[derive(Deserialize)]
struct OAuthCallbackCode {
    pub code: String,
}

#[derive(Deserialize, Serialize)]
struct Profile {
    username: String,
}
