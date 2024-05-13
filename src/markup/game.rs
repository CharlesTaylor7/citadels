use super::base;
use maud::{html, Markup};

pub fn page() -> Markup {
    base::page(
        html! {
            (base::htmx_scripts())
            script src="https://unpkg.com/hyperscript.org@0.9.12" { }
            script src="https://unpkg.com/interactjs/dist/interact.min.js" { }
        },
        html! {},
    )
}
