use super::base;
use crate::server::{routes::dev::Profile, state::AppState};
use maud::{html, Markup};
use tower_cookies::Cookies;

pub fn page(cookies: &Cookies, users: &[Profile]) -> Markup {
    let empty = Profile {
        username: "".to_owned(),
        email: "".to_owned(),
    };
    base::page(
        cookies,
        html! {
            (profile_form(&empty))
            @for profile in users {
                (profile_form(profile))
            }
        },
    )
}

pub fn profile_form(profile: &Profile) -> Markup {
    html! {
        form hx-post="/dev/create-profile" hx-swap="none" {
            label {
                "Email"
                input.input.input-primary
                type="text" required
                name="email" value=(profile.email);
            }

            label {
                "Username"
                input.input.input-primary
                    hx-trigger="input delay:500ms"
                    hx-post="/dev/create-profile"
                    type="text" required
                    name="username" value=(profile.username);
            }

            button.btn.btn-primary type="submit" name="impersonate" {
                "Impersonate"
            }
        }
    }
}
