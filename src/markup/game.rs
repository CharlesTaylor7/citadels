use super::base;
use maud::{html, Markup};

pub fn page() -> Markup {
    base::page(
        html! {
            (base::scripts())
        },
        html! {},
    )
}