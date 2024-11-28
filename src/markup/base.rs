use maud::{html, Markup, DOCTYPE};
use std::env;

pub fn page(head: Markup, main: Markup) -> Markup {
    html! {
        (DOCTYPE)
        html
            hx-ext="ws,morph,json-enc,client-side-templates"
            _="init get the theme of localStorage if it exists set my @data-theme to it" data-theme="dark"
        {
            head {
                title { "Citadels" }
                meta charset="utf-8";
                link name="viewport" content="width=device-width, initial-scale=1";
                link rel="shortcut icon" href=(asset("htmx.png"));
                link rel="stylesheet" href=(asset("styles/index.css"));
                (head)
            }

            body hx-swap="morph" hx-target="body" {
                (main)
            }
        }
    }
}

pub fn nav(logged_in: bool) -> Markup {
    html! {
      div class="flex flex-row justify-end items-center" {
        ul class="menu menu-horizontal bg-base-200 rounded-box" {
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
             a href=(asset("rulebook.pdf")) target="_blank" {
              "Rules"
             }
          }
          li {
            a href="/logout" {
                "Logout"
            }
          }
          li {
            a href="/login" {
                "Login"
            }
          }
          li {
              a href="/signup" {
                  "Signup"
              }
          }
        }
      }
    }
}

pub fn htmx_scripts() -> Markup {
    html! {
        script src="https://unpkg.com/htmx.org@1.9.10/dist/htmx.js" { }
        script src="https://unpkg.com/htmx.org@1.9.10/dist/ext/ws.js" { }
        script src="https://unpkg.com/htmx.org@1.9.10/dist/ext/json-enc.js" { }
        // script src="https://unpkg.com/htmx.org@1.9.10/dist/ext/client-side-templates.js" { }
        script src=(asset("vendor/idiomorph.js")) { }
    }
}

pub fn scripts() -> Markup {
    html! {
        (htmx_scripts())
        script src="https://unpkg.com/hyperscript.org@0.9.12" { }
        script src="https://unpkg.com/interactjs/dist/interact.min.js" { }
    }
}

pub fn asset(path: &str) -> String {
    if cfg!(feature = "dev") {
        format!("/public/{path}")
    } else {
        format!("/public/{path}")
        // TODO: use uploadthing in production
        // format!(
        //     "{}/storage/v1/object/public/assets/{path}",
        // )
    }
}
