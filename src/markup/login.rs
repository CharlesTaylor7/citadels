use super::base;
use crate::markup::base::{htmx_scripts, nav};
use maud::{html, Markup};

pub fn page() -> Markup {
    base::page(
        html! {
            (htmx_scripts())
        },
        html! {
            (nav(false))
            .mt-4 .flex .flex-row .justify-center .items-center {
                form class="gap-4 form-control w-full max-w-xs"  hx-post="/login" hx-swap="none" {
                    label class="input input-bordered flex items-center gap-2" {
                        input type="username" name="email" autocomplete="email" class="grow" placeholder="email" required;
                    }
                    label class="input input-bordered flex items-center gap-2" {
                        input type="password" name="password" autocomplete="current-password" class="grow" placeholder="password" required;
                    }
                    button class="btn btn-primary"{
                        "Login"
                    }
                }
            }
        },
    )
}
