use crate::server::routes::RoomSummary;

use super::base;
use maud::{html, Markup};
use tower_cookies::Cookies;

pub fn page(cookies: &Cookies, rooms: &[RoomSummary]) -> Markup {
    base::page(
        cookies,
        html! {
            button.btn.btn-primary hx-post="/lobby/create-room" hx-swap="none" {
                "Create Room"
            }
            @for room in rooms {
                div.flex.flex-col.items-start {
                    button.btn.btn-primary hx-post="/lobby/join-room" name="room_id" value=(room.id) {
                        "Join Room"
                    }
                    div {
                        "Room:"
                        (room.id)
                    }
                    div {
                        "Host: "
                        (room.host)
                    }
                    div {
                        "Players: "
                        (room.num_players)
                    }
                }
            }
        },
    )
}

pub fn main() -> Markup {
    html! {
        #lobby {
            "lobby"
        }
    }
}
