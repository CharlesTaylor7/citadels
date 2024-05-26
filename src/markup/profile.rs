use super::base;
use maud::{html, Markup};

pub fn page(username: String) -> Markup {
    base::page(
        html! {
            (base::scripts())
        },
        html! {
            (base::nav(true))
            form class="gap-4 form-control w-full max-w-xs" hx-post="/profile" hx-swap="none" {
                label class="input input-bordered flex items-center gap-2" {
                    input
                        class="grow"
                        type="username"
                        required
                        autocomplete="username"
                        name="username"
                        value=(username)
                        placeholder="username";
                }
                button class="btn btn-primary" {
                    "Save Profile"
                }
            }
        },
    )
}
