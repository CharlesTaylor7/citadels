use super::auth::{self, Claims, JwtDecoder};
use super::supabase::SupabaseClient;
use super::ws::WebSockets;
use crate::strings::UserId;
use crate::{game::Game, lobby::Lobby};
use anyhow::anyhow;
use sqlx::postgres::PgPoolOptions;
use sqlx::{PgPool, Postgres, Transaction};
use std::env;
use std::sync::{Arc, Mutex};
use tower_cookies::Cookies;

#[derive(Clone)]
pub struct AppState {
    // TODO: remove these
    pub lobby: Arc<Mutex<Lobby>>,
    pub game: Arc<Mutex<Option<Game>>>,
    // inherently stateless
    pub jwt_decoder: JwtDecoder,
    pub supabase: SupabaseClient,
    // stateful, but transient
    pub ws_connections: Arc<Mutex<WebSockets>>,
    db_pool: PgPool,
}

impl AppState {
    pub async fn new() -> anyhow::Result<AppState> {
        Ok(Self {
            lobby: Default::default(),
            game: Default::default(),
            jwt_decoder: JwtDecoder::default(),
            supabase: SupabaseClient::default(),
            ws_connections: Default::default(),
            db_pool: PgPoolOptions::new()
                .max_connections(30)
                .connect(&env::var("SUPABASE_DB_URL").unwrap())
                .await?,
        })
    }

    pub async fn user_transaction(
        &self,
        cookies: &Cookies,
    ) -> anyhow::Result<Transaction<'static, Postgres>> {
        let mut transaction = self.db_pool.begin().await?;
        let user_id = self.user_id(&cookies).await?;
        // For row level security
        sqlx::query(&format!("set local citadels.user_id = '{user_id}'",))
            .execute(&mut *transaction)
            .await?;
        Ok(transaction)
    }

    /// Decode the signed in user's JWT and verify their claims
    pub fn user_claims(&self, cookies: &Cookies) -> anyhow::Result<Claims> {
        let cookie = cookies
            .get("access_token")
            .ok_or(anyhow!("not logged in"))?;
        self.jwt_decoder.decode(cookie.value())
    }

    pub async fn user_id(&self, cookies: &Cookies) -> anyhow::Result<UserId> {
        let response = self
            .supabase
            .anon()
            .refresh(
                cookies
                    .get("refresh_token")
                    .ok_or(anyhow!("no refresh token"))?
                    .value(),
            )
            .await?;
        cookies.add(auth::cookie(
            "access_token",
            response.access_token,
            time::Duration::HOUR,
        ));
        cookies.add(auth::cookie(
            "refresh_token",
            response.refresh_token,
            time::Duration::WEEK,
        ));
        Ok(response.user.id)
    }

    pub async fn logout(&self, cookies: Cookies) -> anyhow::Result<()> {
        if let Some(mut access_token) = cookies.get("access_token") {
            self.supabase.anon().logout(access_token.value()).await?;
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
