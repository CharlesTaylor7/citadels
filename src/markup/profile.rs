use super::base;
use crate::server::models::Profile;
use maud::{html, Markup};

pub fn page(profile: Option<Profile>) -> Markup {
    base::page(
        html! {
            (base::scripts())
        },
        html! {
            "Foo"
        },
    )
}
