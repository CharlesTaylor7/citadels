use maud::{html, Markup, DOCTYPE};

pub fn page() -> Markup {
    html! {
        (DOCTYPE)
        html data-theme="retro" {
            head {
                title { "Citadels" }
                meta charset="utf-8";
                link name="viewport" content="width=device-width, initial-scale=1";
                link rel="shortcut icon" href="/public/htmx.png";
                link rel="stylesheet" href="/styles/index.css";
            }
            body {
                ul class="menu menu-horizontal bg-base-200 rounded-box" {
                    li {
                        a href="/public/rulebook.pdf" target="_blank" {
                            "Rules 3"
                        }
                    }
                    /*
                       li {
                       "Home"
                       }
                       li {
                       "Open Rooms"
                       }
                       li {
                       "Active Games"
                       }
                       */
                }
            }
        }
    }
}
/*
<script src="https://unpkg.com/htmx.org@1.9.10/dist/htmx.js"></script>
<script src="https://unpkg.com/htmx.org@1.9.10/dist/ext/ws.js"></script>
<script src="https://unpkg.com/htmx.org@1.9.10/dist/ext/json-enc.js"></script>
<script src="https://unpkg.com/htmx.org@1.9.10/dist/ext/client-side-templates.js"></script>

<script src="https://unpkg.com/hyperscript.org@0.9.12"></script>
<script src="https://unpkg.com/interactjs/dist/interact.min.js"></script>
<!--
Working around an idiomorph bug: https://github.com/bigskysoftware/idiomorph/pull/32
<script src="https://unpkg.com/idiomorph/dist/idiomorph-ext.js"></script>
-->
<script src="/public/vendor/idiomorph.js"></script>
<
*/
