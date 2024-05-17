use crate::strings::{AccessToken, RefreshToken, UserId};
use arcstr::ArcStr;
use reqwest::Response;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, env};

#[derive(Serialize, Deserialize, Debug)]
pub struct EmailCreds<'a> {
    pub email: Cow<'a, str>,
    pub password: Secret<Cow<'a, str>>,
}

#[derive(Serialize)]
pub struct RefreshTokenBody {
    refresh_token: RefreshToken,
}

#[derive(Deserialize)]
pub struct SignInResponse {
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
    pub expires_in: u64,
    pub user: UserSignInResponse,
}

#[derive(Deserialize)]
pub struct UserSignInResponse {
    pub id: UserId,
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

impl<'de, T: Deserialize<'de>> Deserialize<'de> for Secret<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self(T::deserialize(deserializer)?))
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
    pub url: ArcStr,
    pub api_key: ArcStr,
}

impl SupabaseAnonClient {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::new(),
            url: env::var("SUPABASE_PROJECT_URL").unwrap().into(),
            api_key: env::var("SUPABASE_ANON_KEY").unwrap().into(),
        }
    }

    pub async fn signup_email(&self, creds: &EmailCreds<'_>) -> anyhow::Result<SignInResponse> {
        let response: Response = self
            .client
            .post(&format!("{}/auth/v1/signup", self.url))
            .header("apikey", self.api_key.as_str())
            .header("Content-Type", "application/json")
            .json(creds)
            .send()
            .await?;
        let json = response.json::<SignInResponse>().await?;
        Ok(json)
    }

    pub async fn signin_email(&self, creds: &EmailCreds<'_>) -> anyhow::Result<SignInResponse> {
        let response = self
            .client
            .post(&format!("{}/auth/v1/token?grant_type=password", self.url))
            .header::<_, &str>("apikey", self.api_key.as_ref())
            .header("Content-Type", "application/json")
            .json(creds)
            .send()
            .await?;
        let json = response.json::<SignInResponse>().await?;
        Ok(json)
    }

    pub async fn refresh(&self, refresh_token: RefreshToken) -> anyhow::Result<SignInResponse> {
        let data = self
            .client
            .post(&format!(
                "{}/auth/v1/token?grant_type=refresh_token",
                self.url
            ))
            .header("apikey", self.api_key.as_str())
            .header("Content-Type", "application/json")
            .json(&RefreshTokenBody { refresh_token })
            .send()
            .await?
            .json()
            .await?;
        Ok(data)
    }

    pub async fn logout(&self, access_token: &AccessToken) -> anyhow::Result<()> {
        self.client
            .post(&format!("{}/auth/v1/logout", self.url))
            .header("apikey", self.api_key.as_str())
            .header("Content-Type", "application/json")
            .bearer_auth(access_token)
            .send()
            .await?;
        Ok(())
    }
}

/*
use jsonwebtoken::{Algorithm, DecodingKey};
#[derive(Clone, Debug, Deserialize)]
pub struct Claims {
    pub sub: String,
    pub email: String,
    pub exp: usize,
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
*/
