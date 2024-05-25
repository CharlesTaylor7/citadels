use super::base;
use crate::server::models::Profile;
use maud::{html, Markup};

pub fn page(profile: Profile) -> Markup {
    base::page(
        html! {
            (base::scripts())
        },
        html! {
            (base::nav(true))
            form class="gap-4 form-control w-full max-w-xs" hx-post="/signup" {
                label class="input input-bordered flex items-center gap-2" {
                    input type="username" name="username" autocomplete="username" class="grow" placeholder="username" required;
                }
            }
        },
    )
}
