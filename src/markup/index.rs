use super::base;
use maud::{html, Markup};

pub fn page() -> Markup {
    base::page(
        html!(),
        html! {
            (base::nav(false))
        },
    )
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
