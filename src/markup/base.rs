use maud::{html, Markup, DOCTYPE};
use std::env;
use tower_cookies::Cookies;

pub fn page(cookies: &Cookies, main: Markup) -> Markup {
    let logged_in = cookies.get("refresh_token").is_some();
    html! {
        (DOCTYPE)
        html
            class="h-full"
            hx-ext="ws,morph,json-enc"
        {
            head {
                title { "Citadels" }
                meta charset="utf-8";
                link name="viewport" content="width=device-width, initial-scale=1";
                link rel="shortcut icon" href=(asset("htmx.png"));
                link rel="stylesheet" href=(asset("styles/index.css"));
                // htmx
                script src="https://unpkg.com/htmx.org@1.9.10/dist/htmx.js" { }
                script src="https://unpkg.com/htmx.org@1.9.10/dist/ext/ws.js" { }
                script src="https://unpkg.com/htmx.org@1.9.10/dist/ext/json-enc.js" { }
                script src=(asset("vendor/idiomorph.js")) { }
                // hyperscript
                script src="https://unpkg.com/hyperscript.org@0.9.12" { }
                // for drag n' drop
                // script src="https://unpkg.com/interactjs/dist/interact.min.js" { }
            }

            body
                class="h-full"
                data-theme="dark"
                _="init get the theme of localStorage if it exists set my @data-theme to it"
                hx-swap="morph" hx-target="body" {
                (nav(logged_in))
                (main)
            }
        }
    }
}

fn nav(logged_in: bool) -> Markup {
    //let dev = cfg!(feature = "dev");
    html! {
      div class="flex flex-row justify-end items-center" {
        ul class="menu menu-horizontal bg-base-200 rounded-box" {
          @if cfg!(feature = "dev") {
              li {
                 a href="/dev" {
                  "Dev"
                 }
              }
          }
          li {
             a href="/game" {
              "Game"
             }
          }
          li {
             a href="/lobby" {
              "Lobby"
             }
          }

          li {
             a href="/profile" {
              "Profile"
             }
          }
          li {
             a href=(asset("rulebook.pdf")) target="_blank" {
              "Rules"
             }
          }
          li {
              @if logged_in {
                form method="post" action="/oauth/logout" {
                    button type="submit" {
                        "Log out"
                    }
                }
              }
              @else {
                    a href="/login" {
                      "Log in"
                  }
              }
          }
        }
      }
    }
}

pub fn asset(path: &str) -> String {
    if cfg!(feature = "dev") {
        format!("/public/{path}")
    } else {
        format!(
            "{}/storage/v1/object/public/assets/{path}",
            env::var("SUPABASE_PROJECT_URL").unwrap()
        )
    }
}

pub fn postgrest(path: &str) -> String {
    format!("{}/{path}", env::var("SUPABASE_PROJECT_URL").unwrap())
}
