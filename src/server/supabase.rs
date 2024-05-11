use anyhow;
use jsonwebtoken::{Algorithm, DecodingKey};
use reqwest::Client;
use reqwest::Response;
use serde::{Deserialize, Serialize};
use std::env;
use std::sync::Arc;

#[derive(Serialize)]
pub struct EmailCreds<'a> {
    pub email: &'a str,
    pub password: Secret<&'a str>,
}

#[derive(Serialize)]
pub struct RefreshToken<'a> {
    refresh_token: &'a str,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Claims {
    pub sub: String,
    pub email: String,
    pub exp: usize,
}

#[derive(Deserialize)]
pub struct SignInResponse {
    access_token: String,
    refresh_token: String,
    expires_at: usize,
}

#[derive(Clone)]
pub struct Secret<T>(pub T);

impl<T: Serialize> Serialize for Secret<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}
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
    pub url: Arc<str>,
    pub api_key: Arc<str>,
}

impl SupabaseAnonClient {
    pub fn new() -> Self {
        Self {
            client: Client::new(),
            url: env::var("SUPABASE_PROJECT_URL").unwrap().into(),
            api_key: env::var("SUPABASE_ANON_KEY").unwrap().into(),
        }
    }

    pub async fn signup_email(
        &self,
        session_id: &str,
        creds: &EmailCreds<'_>,
    ) -> anyhow::Result<SupabaseSession> {
        let response: Response = self
            .client
            .post(&format!("{}/auth/v1/signup", self.url))
            .header("apikey", self.api_key.as_ref())
            .header("Content-Type", "application/json")
            .json(creds)
            .send()
            .await?;
        let json = response.json::<SignInResponse>().await?;
        let client = SupabaseSession {
            session_id: session_id.to_owned(),
            anon: self.clone(),
            access_token: json.access_token,
            refresh_token: json.refresh_token,
            expires_at: json.expires_at,
        };
        Ok(client)
    }

    pub async fn signin_email(
        &self,
        session_id: &str,
        creds: &EmailCreds<'_>,
    ) -> anyhow::Result<SupabaseSession> {
        let response = self
            .client
            .post(&format!("{}/auth/v1/token?grant_type=password", self.url))
            .header("apikey", self.api_key.as_ref())
            .header("Content-Type", "application/json")
            .json(creds)
            .send()
            .await?;
        let json = response.json::<SignInResponse>().await?;
        let client = SupabaseSession {
            session_id: session_id.to_owned(),
            anon: self.clone(),
            access_token: json.access_token,
            refresh_token: json.refresh_token,
            expires_at: json.expires_at,
        };
        Ok(client)
    }

    pub async fn refresh(&self, refresh_token: &str) -> anyhow::Result<SignInResponse> {
        let data = self
            .client
            .post(&format!(
                "{}/auth/v1/token?grant_type=refresh_token",
                self.url
            ))
            .header("apikey", self.api_key.as_ref())
            .header("Content-Type", "application/json")
            .json(&RefreshToken { refresh_token })
            .send()
            .await?
            .json()
            .await?;
        Ok(data)
    }
}

pub struct SupabaseSession {
    pub session_id: String,
    anon: SupabaseAnonClient,
    access_token: String,
    pub refresh_token: String,
    pub expires_at: usize, // utc epoch in seconds
}

impl SupabaseSession {
    pub fn update(&mut self, response: SignInResponse) {
        self.refresh_token = response.refresh_token;
        self.access_token = response.access_token;
        self.expires_at = response.expires_at;
    }

    pub async fn refresh(&self) -> anyhow::Result<SignInResponse> {
        let data = self
            .anon
            .client
            .post(&format!(
                "{}/auth/v1/token?grant_type=refresh_token",
                self.anon.url
            ))
            .header("apikey", self.anon.api_key.as_ref())
            .header("Content-Type", "application/json")
            .json(&RefreshToken {
                refresh_token: &self.refresh_token,
            })
            .send()
            .await?
            .json()
            .await?;
        Ok(data)
    }

    pub async fn logout(&self) -> anyhow::Result<()> {
        let response = self
            .anon
            .client
            .post(&format!("{}/auth/v1/logout", self.anon.url))
            .header("apikey", self.anon.api_key.as_ref())
            .header("Content-Type", "application/json")
            .bearer_auth(&self.access_token)
            .send()
            .await?;

        std::fs::write("./sample-logout.json", response.text().await?)?;
        Ok(())
    }
}

pub struct JwtDecoder {
    pub secret: Secret<jsonwebtoken::DecodingKey>,
    pub validation: jsonwebtoken::Validation,
}

impl JwtDecoder {
    pub fn new() -> Self {
        Self {
            validation: jsonwebtoken::Validation::new(Algorithm::HS256),
            secret: Secret::new(DecodingKey::from_secret(
                env::var("SUPABASE_JWT_SECRET").unwrap().as_ref(),
            )),
        }
    }

    pub async fn decode(&self, jwt: &str) -> anyhow::Result<Claims> {
        let token = jsonwebtoken::decode::<Claims>(&jwt, &self.secret.0, &self.validation)?;
        Ok(token.claims)
    }
}
