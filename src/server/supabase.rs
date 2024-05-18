use crate::server::state::OAuthCallbackCode;
use crate::strings::{AccessToken, OAuthCode, OAuthCodeVerifier, RefreshToken, UserId};
use arcstr::ArcStr;
use reqwest::Response;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::{borrow::Cow, env};
use thiserror::Error;

use super::state::Signin;

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
    pub async fn exchange_code_for_session(
        &self,
        body: ExchangeOAuthCode,
    ) -> anyhow::Result<DiscordSigninResponse> {
        let response: Response = self
            .client
            .post(&format!("{}/auth/v1/token?grant_type=pkce", self.url))
            .header("apikey", self.api_key.as_str())
            .header("Content-Type", "application/json")
            .json(&body)
            .send()
            .await?;

        let body = response.bytes().await?;
        log::info!("{}", String::from_utf8(body.to_vec())?);
        std::fs::write("sample.json", body.clone())?;
        let json = serde_json::from_slice::<SupabaseResponse<DiscordSigninResponse>>(&body)?;

        let json: Result<_, _> = json.into();
        let json = json?;
        Ok(json)
    }

    pub async fn refresh(&self, refresh_token: RefreshToken) -> anyhow::Result<Signin> {
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
/* DTOS */

#[derive(Debug, Serialize)]
pub struct ExchangeOAuthCode {
    pub auth_code: OAuthCode,
    pub code_verifier: OAuthCodeVerifier,
}

#[derive(Debug, Deserialize)]
pub struct JwtClaims {
    pub user_metadata: UserMetadata,
}

#[derive(Debug, Deserialize)]
pub struct UserMetadata {
    pub full_name: String,
}

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
pub struct DiscordSigninResponse {
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
    pub expires_in: u64,
    pub user: SupabaseUser,
    pub user_metadata: serde_json::Value,
    //pub user_metadata: DiscordUserMetadata,
}

#[derive(Deserialize, Debug)]
pub struct DiscordUserMetadata {}

#[derive(Deserialize, Debug)]
pub struct SupabaseUser {
    pub id: UserId,
}

/* Supabase utility types */
#[derive(Deserialize, Debug, Error)]
#[serde(untagged)]
pub enum SupabaseError {
    #[error("Supabase Error: {error}\n{error_description}")]
    A {
        error: String,
        error_description: String,
    },
    #[error("Supabase Status Code {code}. Reason: {error_code}\n{msg}")]
    B {
        error_code: String,
        code: u16,
        msg: String,
    },
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
