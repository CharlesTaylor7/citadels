use super::auth;
use super::ws::WebSockets;
use crate::strings::UserId;
use crate::{game::Game, lobby::Lobby};
use anyhow::{anyhow, bail};
use arcstr::ArcStr;
use sqlx::postgres::PgPoolOptions;
use sqlx::{PgPool, Postgres, Transaction};
use std::borrow::Cow;
use std::env;
use std::sync::{Arc, Mutex};
use tower_cookies::cookie::SameSite;
use tower_cookies::{Cookie, Cookies};

#[derive(Clone)]
pub struct AppState {
    pub client: reqwest::Client,
    pub ws_connections: Arc<Mutex<WebSockets>>,
    cookie_signing_key: tower_cookies::Key,
    db_pool: PgPool,
}

impl AppState {
    pub async fn new() -> anyhow::Result<AppState> {
        Ok(Self {
            cookie_signing_key: tower_cookies::Key::from(
                env::var("COOKIE_SIGNING_KEY").unwrap().as_bytes(),
            ),
            client: reqwest::Client::new(),
            ws_connections: Default::default(),
            db_pool: PgPoolOptions::new()
                .max_connections(30)
                .connect(&env::var("DATABASE_URL").unwrap())
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

    pub async fn add_cookie(
        &self,
        cookies: &Cookies,
        name: impl Into<Cow<'static, str>>,
        value: impl Into<Cow<'static, str>>,
        duration: time::Duration,
    ) {
        cookies
            .signed(&self.cookie_signing_key)
            .add(auth::cookie(name, value, duration))
    }

    pub async fn user_id(&self, cookies: &Cookies) -> anyhow::Result<UserId> {
        Ok(UserId::new(
            cookies
                .signed(&self.cookie_signing_key)
                .get("user_id")
                .ok_or(anyhow::anyhow!("not logged in"))?
                .value(),
        ))
    }

    pub async fn logout(&self, cookies: &Cookies) -> anyhow::Result<()> {
        // TODO: remove access tokens
        auth::remove_cookie(cookies, "user_id");
        Ok(())
    }
}
