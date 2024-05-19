use jsonwebtoken::{Algorithm, DecodingKey};
use serde::Deserialize;
use std::borrow::Cow;
use std::env;
use time::Duration;
use tower_cookies::cookie::SameSite;
use tower_cookies::Cookie;

pub fn cookie<'a>(
    name: impl Into<Cow<'a, str>>,
    value: impl Into<Cow<'a, str>>,
    duration: Duration,
) -> Cookie<'a> {
    let mut cookie = Cookie::build((name, value))
        .max_age(duration)
        .http_only(true);

    #[cfg(not(feature = "dev"))]
    {
        cookie = cookie.secure(true).same_site(SameSite::Strict);
    }

    cookie.into()
}

#[derive(Clone)]
pub struct JwtDecoder {
    pub secret: jsonwebtoken::DecodingKey,
    pub validation: jsonwebtoken::Validation,
}

impl Default for JwtDecoder {
    fn default() -> Self {
        let mut validation = jsonwebtoken::Validation::new(Algorithm::HS256);
        validation.set_audience(&["authenticated"]);
        Self {
            validation,
            secret: DecodingKey::from_secret(env::var("SUPABASE_JWT_SECRET").unwrap().as_ref()),
        }
    }
}

impl JwtDecoder {
    pub fn decode(&self, jwt: &str) -> anyhow::Result<Claims> {
        let token = jsonwebtoken::decode::<Claims>(&jwt, &self.secret, &self.validation)?;
        Ok(token.claims)
    }
}
type Claims = serde_json::Value;

#[derive(Deserialize)]
pub struct Claims_ {
    aud: String, // Optional. Audience
    exp: usize, // Required (validate_exp defaults to true in validation). Expiration time (as UTC timestamp)
    //iat: usize, // Optional. Issued at (as UTC timestamp)
    iss: String, // Optional. Issuer
    //nbf: usize, // Optional. Not Before (as UTC timestamp)
    sub: String,
}
