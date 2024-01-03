use crate::actions::ActionTag;
use crate::actions::ActionTag::*;
use crate::game::Game;
use crate::game::PlayerInfo;
use crate::types::Character;
use crate::types::UniqueDistrict::*;
use crate::{game, lobby};
use askama::Template;
use std::borrow::Borrow;

use axum::response::Html;

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum GamePhase {
    Draft,
    Call,
}

#[derive(Template)]
#[template(path = "game/index.html")]
pub struct GameTemplate<'a> {
    debug: bool,
    phase: GamePhase,
    draft: &'a [Character],
    draft_discard: &'a [Character],
    allowed_actions: &'a [ActionTag],
    characters: &'a [Character],
    players: &'a [PlayerInfo<'a>],
    active_id: Option<&'a str>,
    my: &'a game::Player,
}

impl<'a> GameTemplate<'a> {
    pub fn render<'b>(
        game: &'a Game,
        player_id: Option<&'b str>,
    ) -> axum::response::Result<Html<String>> {
        let def = game::Player::default();
        let player = player_id
            .and_then(|id| game.players.iter().find(|p| p.id == id))
            .unwrap_or(&def);
        let players: Vec<_> = game.players.iter().map(game::Player::info).collect();
        let rendered = GameTemplate {
            characters: game.characters.borrow(),
            draft: game.draft.remaining.borrow(),
            draft_discard: game.draft.faceup_discard.borrow(),
            players: players.borrow(),
            allowed_actions: game.allowed_actions().borrow(),
            active_id: game.active_player().map(|p| p.id.borrow()),
            my: player.borrow(),
            debug: cfg!(feature = "dev"),
            phase: match game.active_turn {
                game::Turn::Draft(_) => GamePhase::Draft,
                game::Turn::Call(_) => GamePhase::Call,
            },
        }
        .render()
        .map_err(|e| format!("askama error: {}", e))?;

        Ok(Html(rendered))
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
