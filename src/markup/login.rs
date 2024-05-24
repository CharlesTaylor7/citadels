use super::base;
use crate::markup::base::{asset, nav, scripts};
use maud::{html, Markup};

pub fn page() -> Markup {
    base::page(
        html! {
            (scripts())
        },
        html! {
            (nav(false))
            .mt-4 .flex .flex-row .justify-center .items-center {
                a
                    href="/oauth/signin?provider=discord"
                    class="btn btn-primary"
                {
                    // todo:
                    // Consider changing icon color depending on user's theme
                    // https://stackoverflow.com/questions/2182716/is-it-possible-to-set-a-src-attribute-of-an-img-tag-in-css
                    // https://discord.com/branding
                    img id="discord-icon" height="20" width="20" src=(asset("brands/discord-mark-white.svg"))

                    span {
                        "Sign in with Discord"
                    }
                }
            }
        },
    )
}
