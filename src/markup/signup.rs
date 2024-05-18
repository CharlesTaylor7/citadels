use super::base;
use crate::markup::base::{nav, scripts};
use maud::{html, Markup, PreEscaped};

pub fn page() -> Markup {
    base::page(
        html! {
            (scripts())
            script type="text/hyperscript" {
              (PreEscaped("
                behavior PasswordField
                    on input
                        log 'input'
                        set password to the first <input[name='password']/>
                        set confirm to the first <input[name='confirm_password']/>
                        if password.value is confirm.value
                            setCustomValidity('') on confirm
                            reportValidity() on confirm
                        end
                    end
                    on change
                        log 'change'
                        set password to the first <input[name='password']/>
                        set confirm to the first <input[name='confirm_password']/>

                        if password.value.length and confirm.value.length
                            setCustomValidity('Passwords do not match') on confirm
                        end

                        if password.value.length
                            reportValidity() on password
                        end

                        if confirm.value.length
                            reportValidity() on confirm
                        end
                    end
                  end
              "))
            }
        },
        html! {
            (nav(false))
            .mt-4 .flex .flex-row .justify-center .items-center {
                form class="gap-4 form-control w-full max-w-xs" hx-post="/signup" {
                    label class="input input-bordered flex items-center gap-2" {
                        input type="username" name="username" autocomplete="username" class="grow" placeholder="username" required;
                    }
                    label class="input input-bordered flex items-center gap-2" {
                        input type="username" name="email" autocomplete="email" class="grow" placeholder="email" required;
                    }
                    label class="input input-bordered flex items-center gap-2" {
                        input type="password" name="password" autocomplete="new-password" class="grow" placeholder="password" required minlength="6" _="install PasswordField";
                    }

                    label class="input input-bordered flex items-center gap-2"
                                            {
                        input type="password" name="confirm_password" autocomplete="new-password" class="grow" placeholder="confirm password" required minlength="6" _="install PasswordField";

                    }
                    button id="signup" class="btn btn-primary" {
                        "Signup"
                    }
                }
            }
        },
    )
}
