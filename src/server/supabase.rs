use crate::strings::{AccessToken, RefreshToken, UserId};
use arcstr::ArcStr;
use reqwest::Response;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, env};
use thiserror::Error;

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
        let json = response.json::<SupabaseResponse<SignInResponse>>().await?;

        log::info!("{:#?}", json);
        let json: Result<_, _> = json.into();
        let json = json?;
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

/* DTOS */
#[derive(Serialize, Deserialize)]
pub struct EmailCreds<'a> {
    pub email: Cow<'a, str>,
    pub password: Cow<'a, str>,
}

#[derive(Serialize)]
pub struct RefreshTokenBody {
    refresh_token: RefreshToken,
}

#[derive(Deserialize, Debug)]
pub struct SignInResponse {
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
    pub expires_in: u64,
    pub user: UserSignInResponse,
}

#[derive(Deserialize, Debug)]
pub struct UserSignInResponse {
    pub id: UserId,
}

/* Supabase utility types */
#[derive(Deserialize, Debug, Error)]
#[error("{error}, {error_description}")]
pub struct SupabaseError {
    error: String,
    error_description: String,
}
#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum SupabaseResponse<T> {
    Success(T),
    Error(SupabaseError),
}

impl<T> From<SupabaseResponse<T>> for Result<T, SupabaseError> {
    fn from(value: SupabaseResponse<T>) -> Self {
        match value {
            SupabaseResponse::Success(value) => Ok(value),
            SupabaseResponse::Error(value) => Err(value),
        }
    }
}
/*
impl<T> Into<Result<T, SupabaseError>> for SupabaseResponse<T> {
    fn into(self) -> Result<T, SupabaseError> {
        match self {
            SupabaseResponse::Success(value) => Ok(value),
            SupabaseResponse::Error(value) => Err(value),
        }
    }
}
*/
/*
impl<T> TryInto<T> for SupabaseResponse<T> {
    type Error = ();
    fn try_into(self) -> Result<T, <Self as TryInto<T>>::Error> {
        Ok(todo!())
    }
}
*/
/*
impl<T> TryFrom<SupabaseResponse<T>> for T {
    type Error = SupabaseError;
    fn try_from(value: SupabaseResponse<T>) -> Result<Self, Self::Error> {
        match value {
            SupabaseResponse::Success(value) => Ok(value),
            SupabaseResponse::Error(value) => Err(value),
        }
    }
}
*/
