use anyhow;
use jsonwebtoken::{Algorithm, DecodingKey};
use reqwest::Client;
use reqwest::Response;
use serde::{Deserialize, Serialize};
use std::env;

#[derive(Serialize, Deserialize)]
pub struct BasicAuth<'a> {
    email: &'a str,
    password: &'a str,
}

#[derive(Serialize, Deserialize)]
pub struct RefreshToken<'a> {
    refresh_token: &'a str,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Claims {
    pub sub: String,
    pub email: String,
    pub exp: usize,
}

#[derive(Clone)]
pub struct Secret<T>(pub T);
impl<T> Secret<T> {
    pub fn new(item: T) -> Self {
        Self(item)
    }
}

impl<T> std::fmt::Debug for Secret<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Secret")
    }
}

#[derive(Clone, Debug)]
pub struct SupabaseAnonClient {
    pub client: reqwest::Client,
    pub url: String,
    pub api_key: String,
    pub jwt_secret: Secret<jsonwebtoken::DecodingKey>,
    pub jwt_validation: jsonwebtoken::Validation,
}

#[derive(Clone)]
pub struct SupabaseClient {
    bearer_token: String,
    anon: SupabaseAnonClient,
}

impl SupabaseAnonClient {
    pub fn new() -> Self {
        Self {
            client: Client::new(),
            url: env::var("SUPABASE_PROJECT_URL").unwrap(),
            api_key: env::var("SUPABASE_ANON_KEY").unwrap(),
            jwt_validation: jsonwebtoken::Validation::new(Algorithm::HS256),
            jwt_secret: Secret::new(DecodingKey::from_secret(
                env::var("SUPABASE_JWT_SECRET").unwrap().as_ref(),
            )),
        }
    }

    pub async fn signup_email_password(&self, email: &str, password: &str) -> anyhow::Result<()> {
        let response: Response = self
            .client
            .post(&format!("{}/auth/v1/signup", self.url))
            .header("apikey", &self.api_key)
            .header("Content-Type", "application/json")
            .json(&BasicAuth { email, password })
            .send()
            .await?;
        std::fs::write("./sample-signup.json", response.text().await?)?;
        Ok(())
    }

    pub async fn decode_jwt(&self, jwt: &str) -> anyhow::Result<Claims> {
        let token = jsonwebtoken::decode::<Claims>(&jwt, &self.jwt_secret.0, &self.jwt_validation)?;
        Ok(token.claims)
    }

    pub async fn sign_in_password(&self, email: &str, password: &str) -> anyhow::Result<()> {
        let response = self
            .client
            .post(&format!("{}/auth/v1/token?grant_type=password", self.url))
            .header("apikey", &self.api_key)
            .header("Content-Type", "application/json")
            .json(&BasicAuth { email, password })
            .send()
            .await?;
        std::fs::write("./sample-signin.json", response.text().await?)?;
        Ok(())
    }

    pub async fn refresh_token(&self, refresh_token: &str) -> anyhow::Result<()> {
        let response = self
            .client
            .post(&format!(
                "{}/auth/v1/token?grant_type=refresh_token",
                self.url
            ))
            .header("apikey", &self.api_key)
            .header("Content-Type", "application/json")
            .json(&RefreshToken { refresh_token })
            .send()
            .await?;

        std::fs::write("./sample-refresh.json", response.text().await?)?;
        Ok(())
    }
}

impl SupabaseClient {
    pub async fn logout(&self) -> anyhow::Result<()> {
        let response = self
            .anon
            .client
            .post(&format!("{}/auth/v1/logout", self.anon.url))
            .header("apikey", &self.anon.api_key)
            .header("Content-Type", "application/json")
            .bearer_auth(&self.bearer_token)
            .send()
            .await?;

        std::fs::write("./sample-logout.json", response.text().await?)?;
        Ok(())
    }
}
