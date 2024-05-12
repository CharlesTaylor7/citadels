use maud::{html, Markup, DOCTYPE};
use std::env;

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

pub fn page(head: Markup, main: Markup) -> Markup {
    html! {
      (DOCTYPE)
      html data-theme="dark" {
        head {
          title { "Citadels" }
          meta charset="utf-8";
          link name="viewport" content="width=device-width, initial-scale=1";
          link rel="shortcut icon" href=(asset("/htmx.png"));
          link rel="stylesheet" href=(asset("/styles/index.css"));
          (head)
        }
        body {
          (main)
        }
      }
    }
}
pub fn nav(logged_in: bool) -> Markup {
    html! {
      .flex.flex-row.justify-end.items-center {
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
              @if logged_in {
                a href="/logout" {
                    "Logout"
                }
              }
              @else {
                a href="/login" {
                    "Login"
                }
              }
          }
          @if !logged_in {
              li {
                  a href="/signup" {
                      "Signup"
                  }
              }
          }

        }
      }
    }
}
