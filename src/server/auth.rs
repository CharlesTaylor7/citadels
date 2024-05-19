use std::borrow::Cow;
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
