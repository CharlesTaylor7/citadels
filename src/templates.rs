pub mod filters;
use crate::actions::ActionTag;
use crate::districts::DistrictName;
use crate::game::{CityDistrict, FollowupAction, Game, GameRole, Turn};
use crate::roles::RoleName;
use crate::types::{CardSuit, PlayerName};
use crate::{game, lobby};
use askama::Template;
use axum::response::Html;
use log::*;
use std::borrow::{Borrow, Cow};

#[derive(Template)]
#[template(path = "game/city.html")]
pub struct CityRootTemplate<'a> {
    city: CityTemplate<'a>,
}

impl<'a> CityRootTemplate<'a> {
    pub fn from(
        player_name: Cow<'a, PlayerName>,
        game: &'a Game,
        active_id: Option<&'a str>,
    ) -> Result<Self, String> {
        let myself = get_myself(game, active_id);
        let player = game
            .players
            .iter()
            .find(|p| p.name == *player_name)
            .ok_or(format!("no player named: {}", player_name))?;

        let (tooltip_class, header) = if myself.is_some_and(|p| p.id == player.id) {
            ("".into(), "My City".into())
        } else {
            (
                "tooltip-open tooltip-bottom".into(),
                format!("{}'s City", player_name).into(),
            )
        };

        Ok(Self {
            city: CityTemplate {
                header,
                tooltip_class,
                districts: player
                    .city
                    .iter()
                    .map(DistrictTemplate::from_city)
                    .collect::<Vec<_>>(),
            },
        })
    }
}

// for the thief, assassin
#[derive(Template)]
#[template(path = "game/menus/select-role.html")]
pub struct SelectRoleMenu<'a> {
    pub roles: Vec<RoleTemplate>,
    pub action: ActionTag,
    pub header: Cow<'a, str>,
    pub confirm: Cow<'a, str>,
}

// for building
#[derive(Template)]
#[template(path = "game/menus/build.html")]
pub struct BuildMenu {
    pub unit: (),
}

pub struct ImageAssetTemplate {
    height: f64,
    width: f64,
    offset_x: f64,
    offset_y: f64,
    scale_percentage: f64,
    path: &'static str,
}

#[derive(Template)]
#[template(path = "game/index.html")]
pub struct GameTemplate<'a> {
    logs: &'a [String],
    menu: MainTemplate<'a>,
    characters: &'a [GameRole],
    players: &'a [PlayerInfoTemplate<'a>],
    active_name: &'a str,
    my: &'a PlayerTemplate<'a>,
    misc: MiscTemplate,
    city: CityTemplate<'a>,
}

impl<'a> GameTemplate<'a> {
    pub fn render_with<'b, 'c>(
        game: &'a Game,
        player_id: Option<&'b str>,
    ) -> axum::response::Result<Html<String>> {
        let active_player = game.active_player()?;
        let myself = get_myself(game, player_id);
        let player_template = PlayerTemplate::from(myself);
        let players: Vec<_> = game
            .players
            .iter()
            .map(|p| PlayerInfoTemplate::from(p, game))
            .collect();

        GameTemplate {
            logs: &game.logs,
            characters: &game.characters,
            city: CityRootTemplate::from(
                myself.map(|p| Cow::Borrowed(&p.name)).unwrap_or_default(),
                game,
                player_id,
            )?
            .city,
            misc: MiscTemplate {
                round: game.round,
                deck: game.deck.size(),
                timer: None,
            },
            menu: MainTemplate {
                header: Cow::Borrowed({
                    if game.active_turn.draft().is_some() {
                        "Draft"
                    } else if (game.followup).is_some() {
                        "Select"
                    } else {
                        "Actions"
                    }
                }),
                view: MenuView::from(game),
            },
            players: &players,
            active_name: &active_player.name.0,
            my: player_template.borrow(),
        }
        .to_html()
    }
}

#[cfg(feature = "dev")]
fn get_myself<'a, 'b>(game: &'a Game, _player_id: Option<&'b str>) -> Option<&'a game::Player> {
    game.active_player().ok()
}

#[cfg(not(feature = "dev"))]
fn get_myself<'a, 'b>(game: &'a Game, player_id: Option<&'b str>) -> Option<&'a game::Player> {
    player_id.and_then(|id| game.players.iter().find(|p| p.id == id))
}

pub struct MainTemplate<'a> {
    header: Cow<'a, str>,
    view: MenuView,
}

struct MiscTemplate {
    round: usize,
    deck: usize,
    timer: Option<usize>,
}

pub struct CityTemplate<'a> {
    header: Cow<'a, str>,
    districts: Vec<DistrictTemplate>,
    tooltip_class: Cow<'a, str>,
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
    pub city_size: usize,
    pub crowned: bool,
    pub complete_city: bool,
    pub first_complete_city: bool,
}

impl<'a> PlayerInfoTemplate<'a> {
    pub fn from(player: &'a game::Player, game: &'a Game) -> Self {
        Self {
            name: player.name.0.borrow(),
            gold: player.gold,
            hand_size: player.hand.len(),
            city_size: player.city.len(),
            crowned: game.crowned == player.name,
            first_complete_city: game
                .first_to_complete
                .as_ref()
                .is_some_and(|c| c == player.name),
            complete_city: player.city.len() >= game.complete_city_size(),
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
                    .map(DistrictTemplate::from)
                    .collect::<Vec<_>>(),
                roles: p
                    .roles
                    .iter()
                    .map(|r| RoleTemplate::from(*r, 150.0))
                    .collect(),
            }
        } else {
            Self {
                name: "",
                gold: 0,
                hand: Vec::with_capacity(0),
                roles: Vec::with_capacity(0),
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
    pub fn from(district: DistrictName) -> Self {
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
        let mut template = Self::from(district.name);
        template.beautified = district.beautified;
        template
    }
}

pub enum MenuView {
    Draft {
        roles: Vec<RoleTemplate>,
        discard: Vec<RoleTemplate>,
        actions: Vec<ActionTag>,
    },
    Call {
        actions: Vec<ActionTag>,
    },
    Followup {
        action: ActionTag,
        revealed: Vec<DistrictTemplate>,
    },
}

impl MenuView {
    pub fn from(game: &Game) -> Self {
        let actions = game.allowed_actions();
        match game.active_turn {
            Turn::GameOver => MenuView::Call { actions },
            Turn::Draft(_) => MenuView::Draft {
                actions,
                roles: game
                    .draft
                    .remaining
                    .iter()
                    .map(|r| RoleTemplate::from(*r, 200.0))
                    .collect::<Vec<_>>(),
                discard: game
                    .draft
                    .faceup_discard
                    .iter()
                    .map(|r| RoleTemplate::from(*r, 200.0))
                    .collect::<Vec<_>>(),
            },

            Turn::Call(_) => match &game.followup {
                Some(FollowupAction { action, revealed }) => MenuView::Followup {
                    action: *action,
                    revealed: revealed
                        .iter()
                        .cloned()
                        .map(DistrictTemplate::from)
                        .collect(),
                },

                None => MenuView::Call { actions },
            },
        }
    }
}

#[derive(Template)]
#[template(path = "game/logs.html")]
pub struct LogsTemplate<'a> {
    pub logs: &'a [String],
}

pub struct RoleTemplate {
    pub name: String,
    pub rank: u8,
    pub value: String,
    pub suit: Option<CardSuit>,
    pub description: &'static str,
    pub asset: ImageAssetTemplate,
}

impl RoleTemplate {
    pub fn from(role: RoleName, height: f64) -> Self {
        let data = role.data();
        let width = height * 155.0 / 200.0;
        let full_height = height * 265.0 / 200.0;
        Self {
            name: role.display_name(),
            rank: data.rank,
            value: format!("{:#?}", role),
            suit: data.suit,
            description: data.description,
            asset: ImageAssetTemplate {
                path: "/public/roles.jpeg",
                height,
                width,
                scale_percentage: 400.0,
                offset_x: -width * (role as usize % 10) as f64,
                offset_y: -full_height * (role as usize / 10) as f64,
            },
        }
    }
}

pub trait MyTemplate {
    fn to_html(&self) -> axum::response::Result<Html<String>>;
}

impl<T: Template> MyTemplate for T {
    fn to_html(&self) -> axum::response::Result<Html<String>> {
        match self.render() {
            Ok(html) => Ok(Html(html)),
            Err(err) => Err(format!("askama: {}", err).into()),
        }
    }
}
