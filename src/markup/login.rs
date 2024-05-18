use super::base;
use crate::markup::base::{nav, scripts};
use maud::{html, Markup};

pub fn page() -> Markup {
    base::page(
        html! {
            (scripts())
        },
        html! {
            (nav(false))
            .mt-4 .flex .flex-row .justify-center .items-center {
                a href="/oauth/signin?provider=discord"
                    class="btn btn-primary"
                {
                    "Sign in with Discord"
                }
           }
        },
    )
}
