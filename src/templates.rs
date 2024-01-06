use crate::actions::ActionTag;
use crate::actions::ActionTag::*;
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
    actions: &'a [ActionTag],
    characters: Vec<RoleTemplate>,
    players: &'a [PlayerInfoTemplate<'a>],
    active_name: &'a str,
    my: &'a PlayerTemplate<'a>,
}

impl<'a> GameTemplate<'a> {
    pub fn render<'b, 'c>(
        game: &'a Game,
        player_id: Option<&'b str>,
        impersonate: Option<&'c game::PlayerName>,
    ) -> axum::response::Result<Html<String>> {
        let active_player = game.active_player();
        let player = PlayerTemplate::from(myself(game, player_id, impersonate));
        let players: Vec<_> = game.players.iter().map(PlayerInfoTemplate::from).collect();

        let rendered = GameTemplate {
            characters: game
                .characters
                .iter()
                .cloned()
                .map(RoleTemplate::from)
                .collect(),
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
            actions: &game.allowed_actions(),
            active_name: &active_player.ok_or("no active player")?.name.0,
            my: player.borrow(),
            dev_mode: cfg!(feature = "dev"),
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

#[cfg(feature = "dev")]
fn myself<'a, 'b, 'c>(
    game: &'a Game,
    _player_id: Option<&'b str>,
    impersonate: Option<&'c game::PlayerName>,
) -> Option<&'a game::Player> {
    if let Some(name) = impersonate {
        game.players.iter().find(|p| p.name == *name)
    } else {
        game.active_player()
    }
}

#[cfg(not(feature = "dev"))]
fn myself<'a, 'b>(
    game: &'a Game,
    player_id: Option<&'b str>,
    _impersonate: Option<game::PlayerName>,
) -> Option<&'a game::Player> {
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
        if let Some(p) = player {
            Self {
                name: p.name.0.borrow(),
                gold: p.gold,
                hand: p
                    .hand
                    .iter()
                    .cloned()
                    .map(DistrictTemplate::from_hand)
                    .collect::<Vec<_>>(),
                roles: p.roles.iter().cloned().map(RoleTemplate::from).collect(),
                city: p.city.iter().map(|_| ()).collect(),
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
    pub name: &'static str,
    pub cost: usize,
    pub value: String,
    pub suit: CardSuit,
    pub description: Option<&'static str>,
    pub beautified: bool,
    pub image_offset_x: f64,
    pub image_offset_y: f64,
}

impl DistrictTemplate {
    pub fn from_hand(district: DistrictName) -> Self {
        let data = district.data();
        Self {
            name: data.display_name,
            cost: data.cost,
            value: format!("{:#?}", district),
            suit: data.suit,
            description: data.description,
            beautified: false,
            image_offset_x: -125.8 * (district as usize % 10) as f64,
            image_offset_y: -200.0 * (district as usize / 10) as f64,
        }
    }

    pub fn from_city(district: &CityDistrict) -> Self {
        todo!()
    }
}

pub struct RoleTemplate {
    pub name: String,
    pub rank: u8,
    pub value: String,
    pub suit: Option<CardSuit>,
    pub description: &'static str,
    pub image_offset_x: f64,
    pub image_offset_y: f64,
}

impl RoleTemplate {
    pub fn from(role: RoleName) -> Self {
        let data = role.data();
        Self {
            name: role.display_name(),
            rank: data.rank,
            value: format!("{:#?}", role),
            suit: data.suit,
            description: data.description,
            image_offset_x: -155.0 * (role as usize % 10) as f64,
            image_offset_y: -265.0 * (role as usize / 10) as f64,
        }
    }
}
