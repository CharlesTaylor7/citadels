use super::supabase::DiscordSigninResponse;
use super::ws::WebSockets;
use crate::server::supabase::SupabaseAnonClient;
use crate::server::ws;
use crate::strings::UserName;
use crate::strings::{AccessToken, OAuthCode, OAuthCodeVerifier, RefreshToken, SessionId, UserId};
use crate::{game::Game, lobby::Lobby};
use serde::Deserialize;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tokio::sync::RwLock;
use tower_cookies::Cookies;

type PrivateCookieJar = Cookies;

fn new_arc_mutex<T>(item: T) -> Arc<std::sync::Mutex<T>> {
    Arc::new(std::sync::Mutex::new(item))
}

pub struct SessionInfo {
    pub user_id: UserId,
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
    pub expires_in: u64,
}

#[derive(Clone)]
pub struct AppState {
    pub lobby: Arc<std::sync::Mutex<Lobby>>,
    pub game: Arc<std::sync::Mutex<Option<Game>>>,
    pub supabase: SupabaseAnonClient,
    pub logged_in: Arc<RwLock<HashMap<SessionId, UserSession>>>,
    pub ws_connections: Arc<Mutex<WebSockets>>,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            lobby: new_arc_mutex(Lobby::default()),
            game: new_arc_mutex(None),
            supabase: SupabaseAnonClient::new(),
            logged_in: Arc::new(RwLock::new(HashMap::default())),
            ws_connections: Arc::new(Mutex::new(ws::WebSockets::default())),
        }
    }
}

impl AppState {
    pub async fn session(&self, cookies: &PrivateCookieJar) -> Option<UserSession> {
        let session_id = cookies.get("session_id")?;
        self.logged_in
            .read()
            .await
            .get(&SessionId::new(session_id.value()))
            .cloned()
    }

    pub async fn logout(&self, cookies: &PrivateCookieJar) -> anyhow::Result<()> {
        let session_id = cookies
            .get("session_id")
            .ok_or(anyhow::anyhow!("not actually logged in"))?;
        let session_id = SessionId::new(session_id.value());
        let mut lock = self.logged_in.write().await;
        let session = lock
            .remove(&session_id)
            .ok_or(anyhow::anyhow!("lost track of session"))?;
        drop(lock);

        self.supabase.logout(&session.access_token).await?;
        self.ws_connections
            .lock()
            .unwrap()
            .0
            .remove(&session.user_id);
        Ok(())
    }

    pub async fn add_session(&self, session_id: SessionId, signin: DiscordSigninResponse) {
        let session = UserSession {
            access_token: signin.access_token,
            refresh_token: signin.refresh_token,
            username: None,
            user_id: signin.user.id,
        };
        self.logged_in
            .write()
            .await
            .insert(session_id.clone(), session.clone());
    }
}

#[derive(Clone)]
pub struct UserSession {
    pub username: Option<UserName>,
    pub user_id: UserId,
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
}

impl UserSession {
    pub fn update(&mut self, response: Signin) {
        self.access_token = response.access_token;
        self.refresh_token = response.refresh_token;
    }
}

pub fn generate_pkce_pair() -> (String, String) {
    let code_verifier = pkce::code_verifier();
    let code_challenge = pkce::code_challenge(&code_verifier);
    ((code_challenge), (code_verifier))
}
/*
pub type Claims = serde_json::Value;

pub struct JwtDecoder {
    pub secret: jsonwebtoken::DecodingKey,
    pub validation: jsonwebtoken::Validation,
}

impl JwtDecoder {
    pub fn new() -> Self {
        let mut validation = jsonwebtoken::Validation::new(Algorithm::HS256);
        validation.set_audience(&["authenticated"]);
        Self {
            validation,
            secret: DecodingKey::from_secret(env::var("SUPABASE_JWT_SECRET").unwrap().as_ref()),
        }
    }

    pub fn decode(&self, jwt: &str) -> anyhow::Result<Claims> {
        let token = jsonwebtoken::decode::<Claims>(&jwt, &self.secret, &self.validation)?;
        Ok(token.claims)
    }
}
*/

/* DTOs */
#[derive(Deserialize)]
pub struct OAuthCallbackCode {
    pub code: String,
}
#[derive(Deserialize)]
pub struct Signin {
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
    pub expires_in: u64,
}
