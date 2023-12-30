use crate::game::Game;
use crate::types::Character;
use crate::types::District;
use crate::types::UniqueDistrict::*;
use crate::{game, lobby};
use askama::Template;

use axum::response::Html;

#[derive(Template)]
#[template(path = "game/index.html")]
pub struct GameTemplate<'a> {
    debug: bool,
    player_id: &'a str,
    characters: &'a [Character],
    players: &'a [game::Player],
    roles: &'a [Character],
    hand: &'a [District],
}

impl<'a> GameTemplate<'a> {
    pub fn render<'b>(game: &'a Game, player_id: Option<&'b str>) -> Option<Html<String>> {
        let def = game::Player::default();
        let player = player_id
            .and_then(|id| game.players.iter().find(|p| p.id == id))
            .unwrap_or(&def);

        let rendered = GameTemplate {
            characters: &game.characters,
            players: &game.players,
            player_id: &player.id,
            hand: &player.hand,
            roles: &player.roles,
            debug: cfg!(debug_assertions),
        }
        .render()
        .ok()?;

        Some(Html(rendered))
    }
}

#[derive(Template)]
#[template(path = "lobby/index.html")]
pub struct LobbyTemplate<'a> {
    pub username: &'a str,
    pub players: &'a [lobby::Player],
}

#[derive(Template)]
#[template(path = "lobby/players.html")]
pub struct LobbyPlayersTemplate<'a> {
    pub players: &'a [lobby::Player],
}

mod filters {
    use crate::types::CardSuit;

    pub fn suit_bg_character(suit: &Option<CardSuit>) -> askama::Result<&'static str> {
        match suit.as_ref() {
            Some(suit) => suit_bg_color(suit),
            None => Ok("bg-neutral-content"),
        }
    }

    pub fn suit_bg_color(suit: &CardSuit) -> askama::Result<&'static str> {
        Ok(match suit {
            CardSuit::Military => "bg-suit-military",
            CardSuit::Religious => "bg-suit-religious",
            CardSuit::Royal => "bg-suit-royal",
            CardSuit::Trade => "bg-suit-trade",
            CardSuit::Unique => "bg-suit-unique",
        })
    }

    pub fn def<'a>(t: &'a Option<&'static str>) -> askama::Result<&'a str> {
        let c: Option<&str> = t.as_deref();
        Ok(c.unwrap_or_default())
    }
}
