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
                form class="gap-4 form-control w-full max-w-xs" hx-post="/signup" {
                    label class="input input-bordered flex items-center gap-2" {
                        input type="username" name="email" autocomplete="email" class="grow" placeholder="email" required;
                    }
                    label class="input input-bordered flex items-center gap-2" {
                        input type="password" name="password" autocomplete="new-password" class="grow" placeholder="password" required minlength="6";
                    }

                    label class="input input-bordered flex items-center gap-2"
                                            {
                        input type="password" name="confirm_password" autocomplete="new-password" class="grow" placeholder="confirm password" required minlength="6"
                            _="
                                on change
                                    if my.value is the first value of <input[name='password']/>
                                        setCustomValidity('') on me
                                        reportValidity() on me
                                    else 
                                        setCustomValidity('Passwords do not match') on me
                                        reportValidity() on me
                                    end
                                on blur
                                    if my.value is the first value of <input[name='password']/>
                                        remove @disabled from <button#signup/>
                                    end
                                on focus
                                    add @disabled to <button#signup/>
                            ";

                    }
                    button id="signup" class="btn btn-primary" disabled {
                        "Signup"
                    }
                    #error {
                    }
                }
            }
        },
    )
}
