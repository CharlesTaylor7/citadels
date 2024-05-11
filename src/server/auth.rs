use anyhow;
use jsonwebtoken::{Algorithm, DecodingKey, Validation};
use reqwest::Client;
use reqwest::{Error, Response};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct BasicAuth<'a> {
    email: &'a str,
    password: &'a str,
}

#[derive(Serialize, Deserialize)]
pub struct RefreshToken<'a> {
    refresh_token: &'a str,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Claims {
    pub sub: String,
    pub email: String,
    pub exp: usize,
}

impl Clone for Claims {
    fn clone(&self) -> Self {
        Self {
            sub: self.sub.clone(),
            email: self.email.clone(),
            exp: self.exp,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SupabaseAnonClient {
    pub client: reqwest::Client,
    pub url: String,
    pub api_key: String,
    pub jwt: String,
}

#[derive(Clone, Debug)]
pub struct SupabaseClient {
    bearer_token: String,
    anon: SupabaseAnonClient,
}

impl SupabaseAnonClient {
    pub fn new() -> Self {
        let client: Client = Client::new();

        Self {
            client,
            url: env!("SUPABASE_PROJECT_URL").to_string(),
            api_key: env!("SUPABASE_ANON_KEY").to_string(),
            jwt: "TODO".to_owned(),
            //env!("SUPABASE_JWT_SECRET").to_string(),
        }
    }

    pub async fn signup_email_password(&self, email: &str, password: &str) -> anyhow::Result<()> {
        let request_url: String = format!("{}/auth/v1/signup", self.url);
        let response: Response = self
            .client
            .post(&request_url)
            .header("apikey", &self.api_key)
            .header("Content-Type", "application/json")
            .json(&BasicAuth { email, password })
            .send()
            .await?;
        let body = response.json::<serde_json::value::Value>().await;
        log::info!("{:#?}", body);
        Ok(())
    }

    pub async fn decode_jwt(&self, jwt: &str) -> anyhow::Result<Claims> {
        let decoding_key = DecodingKey::from_secret(self.jwt.as_ref());
        let validation = Validation::new(Algorithm::HS256);
        let token = jsonwebtoken::decode::<Claims>(&jwt, &decoding_key, &validation)?;
        Ok(token.claims)
    }

    pub async fn sign_in_password(&self, email: &str, password: &str) -> anyhow::Result<()> {
        let request_url: String = format!("{}/auth/v1/token?grant_type=password", self.url);
        let response: Response = self
            .client
            .post(&request_url)
            .header("apikey", &self.api_key)
            .header("Content-Type", "application/json")
            .json(&BasicAuth { email, password })
            .send()
            .await?;
        let body = response.json::<serde_json::value::Value>().await;
        log::info!("{:#?}", body);
        Ok(())
    }

    pub async fn refresh_token(&self, refresh_token: &str) -> anyhow::Result<()> {
        let request_url: String = format!("{}/auth/v1/token?grant_type=refresh_token", self.url);
        let response: Response = self
            .client
            .post(&request_url)
            .header("apikey", &self.api_key)
            .header("Content-Type", "application/json")
            .json(&RefreshToken { refresh_token })
            .send()
            .await?;

        let body = response.json::<serde_json::value::Value>().await;
        log::info!("{:#?}", body);
        Ok(())
    }
}
impl SupabaseClient {
    pub async fn logout(&self) -> anyhow::Result<()> {
        let request_url: String = format!("{}/auth/v1/logout", self.anon.url);
        let response: Response = self
            .anon
            .client
            .post(&request_url)
            .header("apikey", &self.anon.api_key)
            .header("Content-Type", "application/json")
            .bearer_auth(&self.bearer_token)
            .send()
            .await?;

        let body = response.json::<serde_json::value::Value>().await;
        log::info!("{:#?}", body);
        Ok(())
    }
}
