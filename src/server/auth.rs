use std::borrow::Cow;
use tower_cookies::{cookie::SameSite, Cookie, Cookies};

pub fn remove_cookie(cookies: &Cookies, name: impl Into<Cow<'static, str>>) {
    let mut c = cookie(name, "", time::Duration::ZERO);
    c.make_removal();
    cookies.remove(c)
}

pub fn cookie<'a>(
    name: impl Into<Cow<'a, str>>,
    value: impl Into<Cow<'a, str>>,
    duration: time::Duration,
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
