use super::ws::WebSockets;
use crate::server::supabase::SupabaseAnonClient;
use crate::strings::UserName;
use crate::strings::{AccessToken, RefreshToken, SessionId, UserId};
use crate::{game::Game, lobby::Lobby};
use serde::Deserialize;
use std::sync::{Arc, Mutex};
use tower_cookies::Cookies;

fn new_arc_mutex<T>(item: T) -> Arc<std::sync::Mutex<T>> {
    Arc::new(std::sync::Mutex::new(item))
}

struct SessionInfo {
    pub user_id: UserId,
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
    pub expires_in: u64,
}

#[derive(Default, Clone)]
pub struct AppState {
    pub lobby: Arc<std::sync::Mutex<Lobby>>,
    pub game: Arc<std::sync::Mutex<Option<Game>>>,
    pub supabase: SupabaseAnonClient,
    pub ws_connections: Arc<Mutex<WebSockets>>,
}

impl AppState {
    pub async fn user_id(&self, cookies: Cookies) -> anyhow::Result<UserId> {
        anyhow::bail!("TODO: app.user_id()")
    }
    pub async fn logout(&self, cookies: Cookies) -> anyhow::Result<()> {
        if let Some(mut access_token) = cookies.get("access_token") {
            self.supabase.logout(access_token.value()).await?;
            access_token.make_removal();
        }

        if let Some(mut refresh_token) = cookies.get("refresh_token") {
            refresh_token.make_removal();
        }

        Ok(())
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
