use std::borrow::Cow;
use time::Duration;
use tower_cookies::Cookie;

pub fn cookie<'a>(
    name: impl Into<Cow<'a, str>>,
    value: impl Into<Cow<'a, str>>,
    duration: Duration,
) -> Cookie<'a> {
    let cookie = Cookie::build((name, value))
        .max_age(duration)
        .http_only(true);

    #[cfg(not(feature = "dev"))]
    let cookie = cookie.secure(true);

    cookie.into()
}
