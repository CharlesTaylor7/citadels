use super::models::UserMetadata;
use jsonwebtoken::{Algorithm, DecodingKey};
use serde::Deserialize;
use std::borrow::Cow;
use std::env;
use time::Duration;
use tower_cookies::cookie::SameSite;
use tower_cookies::{Cookie, Cookies};

pub fn remove_cookie(cookies: &Cookies, name: impl Into<Cow<'static, str>>) {
    let mut c = cookie(name, "", time::Duration::ZERO);
    c.make_removal();
    cookies.remove(c)
}

pub fn cookie<'a>(
    name: impl Into<Cow<'a, str>>,
    value: impl Into<Cow<'a, str>>,
    duration: Duration,
) -> Cookie<'a> {
    let mut cookie = Cookie::build((name, value))
        .path("/")
        .max_age(duration)
        .same_site(SameSite::Lax)
        .http_only(true);

    #[cfg(not(feature = "dev"))]
    {
        cookie = cookie.secure(true);
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

#[derive(Deserialize, Debug)]
pub struct Claims {
    pub user_metadata: UserMetadata,
}
