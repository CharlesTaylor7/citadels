use maud::{html, Markup, DOCTYPE};

pub fn page(head: Markup, main: Markup) -> Markup {
  html! {
    (DOCTYPE)
    html data-theme="dark" {
      head {
        title { "Citadels" }
        meta charset="utf-8";
        link name="viewport" content="width=device-width, initial-scale=1";
        link rel="shortcut icon" href="/public/htmx.png";
        link rel="stylesheet" href="/styles/index.css";
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
           a href="/public/rulebook.pdf" target="_blank" {
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
