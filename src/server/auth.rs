use super::supabase::SignInResponse;
use crate::strings::{AccessToken, RefreshToken, SessionId, UserId};
use axum_extra::extract::PrivateCookieJar;
use jsonwebtoken::{Algorithm, DecodingKey};
use std::collections::HashMap;
use std::env;

#[derive(Clone)]
pub struct Session {
    pub session_id: SessionId,
    pub user_id: UserId,
    pub access_token: AccessToken,
    pub refresh_token: RefreshToken,
}

impl Session {
    pub fn update(&mut self, response: SignInResponse) {
        self.access_token = response.access_token;
        self.refresh_token = response.refresh_token;
    }
}

#[derive(Default)]
pub struct Sessions(pub HashMap<SessionId, Session>);

impl Sessions {
    pub fn session_from_cookies(&self, cookies: &PrivateCookieJar) -> Option<Session> {
        let session_id = cookies.get("session_id")?;
        let session_id = SessionId::new(session_id.value());
        self.0.get(&session_id).cloned()
    }

    pub fn session_from_id(&self, session_id: &SessionId) -> Option<Session> {
        self.0.get(session_id).cloned()
    }
}

pub type Claims = serde_json::Value;

pub struct JwtDecoder {
    pub secret: jsonwebtoken::DecodingKey,
    pub validation: jsonwebtoken::Validation,
}

impl JwtDecoder {
    pub fn new() -> Self {
        let mut validation = jsonwebtoken::Validation::new(Algorithm::HS256);
        validation.set_audience(&["authenticated"]);
        Self {
            validation,
            secret: DecodingKey::from_secret(env::var("SUPABASE_JWT_SECRET").unwrap().as_ref()),
        }
    }

    pub fn decode(&self, jwt: &str) -> anyhow::Result<Claims> {
        let token = jsonwebtoken::decode::<Claims>(&jwt, &self.secret, &self.validation)?;
        Ok(token.claims)
    }
}
