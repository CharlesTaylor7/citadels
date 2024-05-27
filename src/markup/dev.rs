use super::base;
use maud::{html, Markup};
use tower_cookies::Cookies;

pub fn page(cookies: &Cookies) -> Markup {
    base::page(
        cookies,
        html! {
                form hx-post="/dev/create-profile" {
                    input type="text" required name="username";
                    input type="text" required name="email";
                    input type="text" required name="password";
                    button type="submit" {
                        "Submit"
                    }
                }
        },
    )
}
