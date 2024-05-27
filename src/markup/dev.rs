use super::base;
use maud::{html, Markup};
use tower_cookies::Cookies;

pub fn page(cookies: &Cookies) -> Markup {
    base::page(
        cookies,
        html! {
            (main())
        },
    )
}

pub fn main() -> Markup {
    html! {
        "dev only page"
    }
}
