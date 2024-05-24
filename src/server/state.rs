use super::auth::JwtDecoder;
use super::ws::WebSockets;
use crate::server::supabase::SupabaseAnonClient;
use crate::strings::UserName;
use crate::strings::{AccessToken, RefreshToken, SessionId, UserId};
use crate::{game::Game, lobby::Lobby};
use anyhow::anyhow;
use serde::Deserialize;
use sqlx::postgres::PgConnectOptions;
use std::sync::{Arc, Mutex};
use tower_cookies::Cookies;

#[derive(Default, Clone)]
pub struct AppState {
    // TODO: remove these
    pub lobby: Arc<Mutex<Lobby>>,
    pub game: Arc<Mutex<Option<Game>>>,
    // inherently stateless
    pub jwt_decoder: JwtDecoder,
    pub supabase: SupabaseAnonClient,
    // stateful, but transient
    pub ws_connections: Arc<Mutex<WebSockets>>,
}

impl AppState {
    pub async fn user_id(&self, cookies: Cookies) -> anyhow::Result<UserId> {
        let cookie = cookies
            .get("access_token")
            .ok_or(anyhow!("no jwt cookie"))?;
        let decoded = self.jwt_decoder.decode(cookie.value());

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

pub fn generate_pkce_pair() -> (String, String) {
    let code_verifier = pkce::code_verifier();
    let code_challenge = pkce::code_challenge(&code_verifier);
    ((code_challenge), (code_verifier))
}
