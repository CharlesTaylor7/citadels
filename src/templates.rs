use crate::actions::ActionTag;
use crate::actions::ActionTag::*;
use crate::districts::DistrictName::*;
use crate::game::Game;
use crate::game::PlayerInfo;
use crate::game::PlayerName;
use crate::roles::RoleName::*;
use crate::types::Role;
use crate::{game, lobby};
use askama::Template;
use axum::response::Html;
use log::*;
use std::borrow::Borrow;
use std::ops::Deref;

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum GamePhase {
    Draft,
    Call,
}

#[derive(Template)]
#[template(path = "game/index.html")]
pub struct GameTemplate<'a> {
    dev_mode: bool,
    phase: GamePhase,
    draft: &'a [&'static Role],
    draft_discard: &'a [&'static Role],
    allowed_actions: &'a [ActionTag],
    characters: &'a [&'static Role],
    players: &'a [PlayerInfo<'a>],
    active_name: &'a PlayerName,
    my: &'a game::Player,
}

impl<'a> GameTemplate<'a> {
    pub fn render<'b>(
        game: &'a Game,
        player_id: Option<&'b str>,
    ) -> axum::response::Result<Html<String>> {
        let active_player = game.active_player();
        let def = game::Player::default();
        let player = my_perspective(game, player_id);
        let player = player.unwrap_or(&def);

        let players: Vec<_> = game.players.iter().map(game::Player::info).collect();

        let rendered = GameTemplate {
            characters: &game.characters,
            draft: game
                .draft
                .remaining
                .iter()
                .map(|role| role.data())
                .collect::<Vec<_>>()
                .borrow(),
            draft_discard: game
                .draft
                .faceup_discard
                .iter()
                .map(|role| role.data())
                .collect::<Vec<_>>()
                .borrow(),
            players: players.borrow(),
            allowed_actions: game.allowed_actions().borrow(),
            active_name: active_player.map_or(def.name.borrow(), |p| p.name.borrow()),
            my: player.borrow(),
            dev_mode: cfg!(feature = "dev"),
            phase: GamePhase::Call,
            /*
             match game.active_turn {
                game::Turn::Draft(_) => GamePhase::Draft,
                game::Turn::Call(_) => GamePhase::Call,
            },
            */
        }
        .render()
        .map_err(|e| format!("askama error: {}", e))?;

        Ok(Html(rendered))
    }
}

#[cfg(feature = "dev")]
fn my_perspective<'a, 'b>(game: &'a Game, _player_id: Option<&'b str>) -> Option<&'a game::Player> {
    if let Some(name) = game.impersonate.borrow() {
        game.players.iter().find(|p| p.name == *name)
    } else {
        game.active_player()
    }
}

#[cfg(not(feature = "dev"))]
fn my_perspective<'a, 'b>(game: &'a Game, player_id: Option<&'b str>) -> Option<&'a game::Player> {
    player_id.and_then(|id| game.players.iter().find(|p| p.id == id))
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
    use std::fmt::{format, Debug};

    use crate::types::CardSuit;

    pub fn debug<T: Debug>(item: &T) -> askama::Result<String> {
        Ok(format!("{:#?}", item))
    }
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
