use super::models::SupabaseUser;
use arcstr::ArcStr;
use reqwest::Response;
use serde::{Deserialize, Serialize};
use std::env;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct SupabaseClient {
    pub client: reqwest::Client,
    pub url: ArcStr,
}

impl Default for SupabaseClient {
    fn default() -> Self {
        Self {
            client: reqwest::Client::new(),
            url: env::var("SUPABASE_PROJECT_URL").unwrap().into(),
        }
    }
}
impl SupabaseClient {
    pub fn anon(&self) -> SupabaseAnonClient {
        SupabaseAnonClient::new(self)
    }

    pub fn service(&self) -> SupabaseServiceClient {
        SupabaseServiceClient::new(self)
    }
}

/// Make requests with elevated permissions
pub struct SupabaseServiceClient {
    client: reqwest::Client,
    url: ArcStr,
    api_key: ArcStr,
}

impl SupabaseServiceClient {
    fn new(supabase: &SupabaseClient) -> Self {
        Self {
            client: supabase.client.clone(),
            url: supabase.url.clone(),
            api_key: env::var("SUPABASE_SERVICE_ROLE_KEY").unwrap().into(),
        }
    }
}

pub struct SupabaseAnonClient {
    client: reqwest::Client,
    url: ArcStr,
    api_key: ArcStr,
}

impl SupabaseAnonClient {
    pub fn new(supabase: &SupabaseClient) -> Self {
        SupabaseAnonClient {
            client: supabase.client.clone(),
            url: supabase.url.clone(),
            api_key: env::var("SUPABASE_ANON_KEY").unwrap().into(),
        }
    }

    pub async fn exchange_code_for_session(
        &self,
        body: ExchangeOAuthCode<'_>,
    ) -> anyhow::Result<OAuthSigninResponse> {
        let response: Response = self
            .client
            .post(&format!("{}/auth/v1/token?grant_type=pkce", self.url))
            .header("apikey", self.api_key.as_str())
            .header("Content-Type", "application/json")
            .json(&body)
            .send()
            .await?;

        let body = response.bytes().await?;
        let json = serde_json::from_slice::<SupabaseResponse<OAuthSigninResponse>>(&body)?
            .into_result()?;
        Ok(json)
    }

    pub async fn refresh(&self, refresh_token: &str) -> anyhow::Result<OAuthSigninResponse> {
        let response = self
            .client
            .post(&format!(
                "{}/auth/v1/token?grant_type=refresh_token",
                self.url
            ))
            .header("apikey", self.api_key.as_str())
            .header("Content-Type", "application/json")
            .json(&RefreshTokenBody { refresh_token })
            .send()
            .await?;

        let body = response.bytes().await?;
        let json = serde_json::from_slice::<SupabaseResponse<OAuthSigninResponse>>(&body)?
            .into_result()?;
        Ok(json)
    }

    pub async fn logout(&self, access_token: &str) -> anyhow::Result<()> {
        self.client
            .post(&format!("{}/auth/v1/logout", self.url))
            //.header("apikey", self.api_key.as_str())
            .header("Content-Type", "application/json")
            .bearer_auth(access_token)
            .send()
            .await?;
        Ok(())
    }
}

/* DTOS */
#[derive(Debug, Serialize)]
pub struct ExchangeOAuthCode<'a> {
    pub auth_code: &'a str,
    pub code_verifier: &'a str,
}

#[derive(Serialize)]
pub struct RefreshTokenBody<'a> {
    refresh_token: &'a str,
}

#[derive(Deserialize, Debug)]
pub struct OAuthSigninResponse {
    pub access_token: String,
    pub refresh_token: String,
    pub user: SupabaseUser,
    pub expires_in: u32,
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
impl<T> SupabaseResponse<T> {
    fn into_result(self) -> Result<T, SupabaseError> {
        match self {
            SupabaseResponse::Success(value) => Ok(value),
            SupabaseResponse::Error(value) => Err(value),
        }
    }
}
