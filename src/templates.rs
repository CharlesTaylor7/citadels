use crate::actions::ActionTag;
use crate::actions::ActionTag::*;
use crate::districts::DistrictName::*;
use crate::districts::{DistrictData, DistrictName};
use crate::game::{CityDistrict, Game};
use crate::roles::RoleName::{self, *};
use crate::types::CardSuit;
use crate::{game, lobby};
use askama::Template;
use axum::response::Html;
use log::*;
use std::borrow::Borrow;
use std::ops::Deref;

#[derive(Template)]
#[template(path = "game/index.html")]
pub struct GameTemplate<'a> {
    dev_mode: bool,
    phase: GamePhase,
    draft: Vec<RoleTemplate>,
    #[allow(unused)]
    draft_discard: Vec<RoleTemplate>,
    allowed_actions: &'a [ActionTag],
    characters: &'a [RoleTemplate],
    players: &'a [PlayerInfoTemplate<'a>],
    active_name: &'a str,
    my: &'a PlayerTemplate<'a>,
}

impl<'a> GameTemplate<'a> {
    pub fn render<'b>(
        game: &'a Game,
        player_id: Option<&'b str>,
    ) -> axum::response::Result<Html<String>> {
        let active_player = game.active_player();
        let player = PlayerTemplate::from(myself(game, player_id));
        let players: Vec<_> = game.players.iter().map(PlayerInfoTemplate::from).collect();

        let rendered = GameTemplate {
            characters: &[],
            draft: game
                .draft
                .remaining
                .iter()
                .cloned()
                .map(RoleTemplate::from)
                .collect::<Vec<_>>(),
            draft_discard: game
                .draft
                .faceup_discard
                .iter()
                .cloned()
                .map(RoleTemplate::from)
                .collect::<Vec<_>>(),
            players: &players,
            allowed_actions: &game.allowed_actions(),
            active_name: &active_player.ok_or("no active player")?.name.0,
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
fn myself<'a, 'b>(game: &'a Game, _player_id: Option<&'b str>) -> Option<&'a game::Player> {
    if let Some(name) = game.impersonate.borrow() {
        game.players.iter().find(|p| p.name == *name)
    } else {
        game.active_player()
    }
}

#[cfg(not(feature = "dev"))]
fn myself<'a, 'b>(game: &'a Game, player_id: Option<&'b str>) -> Option<&'a game::Player> {
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

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum GamePhase {
    Draft,
    Call,
}

/// Just the public player info
pub struct PlayerInfoTemplate<'a> {
    pub name: &'a str,
    pub gold: usize,
    pub hand_size: usize,
    pub city: Vec<()>,
}

impl<'a> PlayerInfoTemplate<'a> {
    pub fn from(player: &'a game::Player) -> Self {
        let game::Player {
            name,
            gold,
            hand,
            city,
            ..
        } = player;
        Self {
            name: name.0.borrow(),
            gold: *gold,
            hand_size: hand.len(),
            // TODO:
            city: city.iter().map(|_| ()).collect(),
        }
    }
}

/// Current player info
#[derive(Default)]
pub struct PlayerTemplate<'a> {
    pub name: &'a str,
    pub gold: usize,
    pub hand: Vec<DistrictTemplate>,
    pub roles: Vec<RoleTemplate>,
    pub city: Vec<()>,
}

impl<'a> PlayerTemplate<'a> {
    pub fn from(player: Option<&'a game::Player>) -> Self {
        if let Some(game::Player {
            name,
            gold,
            hand,
            city,
            ..
        }) = player
        {
            Self {
                name: name.0.borrow(),
                gold: *gold,
                hand: hand
                    .iter()
                    .cloned()
                    .map(DistrictTemplate::from_hand)
                    .collect::<Vec<_>>(),
                roles: Vec::new(),
                city: city.iter().map(|_| ()).collect(),
            }
        } else {
            Self {
                name: "",
                gold: 0,
                hand: Vec::with_capacity(0),
                roles: Vec::with_capacity(0),
                city: Vec::with_capacity(0),
            }
        }
    }
}

pub struct DistrictTemplate {
    pub display_name: &'static str,
    pub cost: usize,
    pub name: DistrictName,
    pub value: String,
    pub suit: CardSuit,
    pub description: Option<&'static str>,
    pub beautified: bool,
    pub image_offset_x: isize,
    pub image_offset_y: isize,
}

impl DistrictTemplate {
    pub fn from_hand(district: DistrictName) -> Self {
        let data = district.data();
        Self {
            display_name: data.display_name,
            cost: data.cost,
            name: data.name,
            value: format!("{:#?}", data),
            suit: data.suit,
            description: data.description,
            beautified: false,
            image_offset_x: -125 * (district as isize % 10),
            image_offset_y: -200 * (district as isize / 10),
        }
    }

    pub fn from_city(district: &CityDistrict) -> Self {
        todo!()
    }
}

pub struct RoleTemplate {
    pub display_name: String,
    pub rank: u8,
    pub name: RoleName,
    pub value: String,
    pub suit: Option<CardSuit>,
    pub description: &'static str,
    pub image_offset_x: isize,
    pub image_offset_y: isize,
}

impl RoleTemplate {
    pub fn from(role: RoleName) -> Self {
        let data = role.data();
        Self {
            display_name: role.display_name(),
            rank: data.rank,
            name: role,
            value: format!("{:#?}", role),
            suit: data.suit,
            description: data.description,
            image_offset_x: -155 * (role as isize % 10),
            image_offset_y: -265 * (role as isize / 10),
        }
    }
}
