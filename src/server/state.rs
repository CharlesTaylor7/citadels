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
            jwt_decoder: JwtDecoder::default(),
            supabase: SupabaseClient::default(),
            ws_connections: Default::default(),
            db_pool: PgPoolOptions::new()
                .max_connections(30)
                .connect(&env::var("SUPABASE_DB_URL").unwrap())
                .await?,
        })
    }

    // bypass row level security
    pub async fn service_transaction(&self) -> anyhow::Result<Transaction<'static, Postgres>> {
        let mut transaction = self.db_pool.begin().await?;

        sqlx::query("SET ROLE service_role")
            .execute(&mut *transaction)
            .await?;
        Ok(transaction)
    }

    /// Signed out user. Just allowed to spectate
    /// Row level security policies will apply.
    pub async fn anon_transaction(&self) -> anyhow::Result<Transaction<'static, Postgres>> {
        let mut transaction = self.db_pool.begin().await?;
        sqlx::query("SET ROLE anon")
            .execute(&mut *transaction)
            .await?;
        Ok(transaction)
    }

    /// Row level security policies will apply.
    pub async fn user_transaction(
        &self,
        cookies: &Cookies,
    ) -> anyhow::Result<Transaction<'static, Postgres>> {
        let mut transaction = self.db_pool.begin().await?;
        let user_id = self.user_id(&cookies).await?;
        sqlx::query(&format!("SET LOCAL citadels.user_id = '{user_id}'"))
            .execute(&mut *transaction)
            .await?;
        sqlx::query("SET ROLE authenticated")
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
        if cfg!(feature = "dev") {
            if let Some(cookie) = cookies.get("impersonate") {
                return Ok(UserId::new(cookie.value()));
            }
        }

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
            time::Duration::seconds(response.expires_in.into()),
        ));
        cookies.add(auth::cookie(
            "refresh_token",
            response.refresh_token,
            time::Duration::WEEK,
        ));
        Ok(response.user.id)
    }

    pub async fn logout(&self, cookies: Cookies) -> anyhow::Result<()> {
        if let Some(access_token) = cookies.get("access_token") {
            let _ = self.supabase.anon().logout(access_token.value()).await;
            auth::remove_cookie(&cookies, "access_token");
        }

        auth::remove_cookie(&cookies, "refresh_token");
        Ok(())
    }
}

pub fn generate_pkce_pair() -> (String, String) {
    let code_verifier = pkce::code_verifier();
    let code_challenge = pkce::code_challenge(&code_verifier);
    ((code_challenge), (code_verifier))
}
