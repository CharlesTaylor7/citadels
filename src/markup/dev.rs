use super::base;
use maud::{html, Markup};
use tower_cookies::Cookies;

pub fn page(cookies: &Cookies) -> Markup {
    base::page(
        cookies,
        html! {
                form hx-post="/dev/create-profile" {
                    label.block {
                        "Username"
                        input.input.input-primary type="text" required name="username";
                    }
                    label.block {
                        "Email"
                        input.input.input-primary type="text" required name="email";
                    }
                    button.btn.btn-primary type="submit" {
                        "Submit"
                    }
                }
        },
    )
}
