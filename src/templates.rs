pub mod filters;
use crate::actions::ActionTag;
use crate::districts::DistrictName;
use crate::game::{CityDistrict, FollowupAction, Game, GameRole, PlayerIndex, Turn};
use crate::roles::{Rank, RoleName};
use crate::types::CardSuit;
use crate::types::Marker;
use crate::{game, lobby};
use askama::Template;
use axum::response::Html;
use std::borrow::{Borrow, BorrowMut, Cow};

#[derive(Template)]
#[template(path = "game/city.html")]
pub struct CityRootTemplate<'a> {
    city: CityTemplate<'a>,
}

impl<'a> CityRootTemplate<'a> {
    pub fn from(game: &'a Game, target: game::PlayerIndex, my_id: Option<&'a str>) -> Self {
        Self {
            city: CityTemplate::from(game, target, my_id),
        }
    }
}

impl<'a> CityTemplate<'a> {
    pub fn from(game: &'a Game, target: game::PlayerIndex, my_id: Option<&'a str>) -> Self {
        let myself = get_myself(game, my_id);

        let (tooltip_class, header) = if myself.is_some_and(|p| p.index == target) {
            ("".into(), "My City".into())
        } else {
            (
                "tooltip-open".into(),
                format!("{}'s City", game.players[target.0].name).into(),
            )
        };

        let mut columns = vec![Vec::new(); 5];
        for card in game.players[target.0].city.iter() {
            //let template = DistrictTemplate::from_city(index, card);
            // unique districts get their own column each
            columns[card.name.data().suit as usize].push(card);
        }

        // sort the non trivial columns
        for col in columns.iter_mut() {
            col.sort_by_key(|d| d.name.data().cost);
        }

        let name = &game.players[target.0].name.0;
        let columns = columns
            .iter()
            .map(|col| {
                col.iter()
                    .enumerate()
                    .map(|(i, card)| DistrictTemplate::from_city(name, i, card))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        let margin_bottom = columns
            .iter()
            .flat_map(|col| col.last())
            .map(|t| t.pos.y)
            .min_by(|a, b| {
                if a < b {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            })
            .unwrap_or(0.0);

        Self {
            header,
            district_tooltip_class: tooltip_class,
            columns,
            margin_bottom,
        }
    }
}

// for the thief, assassin
#[derive(Template)]
#[template(path = "game/menus/select-role.html")]
pub struct SelectRoleMenu<'a> {
    pub roles: Vec<RoleTemplate>,
    pub action: ActionTag,
    pub header: Cow<'a, str>,
    pub context: GameContext,
}

// for building
#[derive(Template)]
#[template(path = "game/menus/build.html")]
pub struct BuildMenu {}

// for magician
#[derive(Template)]
#[template(path = "game/menus/magic.html")]
pub struct MagicMenu {}

#[derive(Template)]
#[template(path = "game/menus/magic-swap-player.html")]
pub struct MagicSwapPlayerMenu<'a> {
    pub players: Vec<&'a str>,
}
#[derive(Template)]
#[template(path = "game/menus/magic-swap-deck.html")]
pub struct MagicSwapDeckMenu {}

#[derive(Template)]
#[template(path = "game/menus/warlord.html")]
pub struct WarlordMenu<'a> {
    pub cities: Vec<CityTemplate<'a>>,
}

#[derive(Template)]
#[template(path = "game/menus/beautify.html")]
pub struct BeautifyMenu;

impl<'a> WarlordMenu<'a> {
    pub fn from_game(game: &'a game::Game) -> Self {
        Self {
            cities: game
                .players
                .iter()
                .filter(|p| {
                    game.active_player_index().is_ok_and(|i| i != p.index)
                        && !p.has_role(RoleName::Bishop)
                })
                .map(|p| CityTemplate::from(&game, p.index, None))
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Clone)]
pub struct ImageAssetTemplate {
    brightness: f64,
    height: f64,
    width: f64,
    offset_x: f64,
    offset_y: f64,
    scale_percentage: f64,
    path: &'static str,
}

pub struct GameContext {
    allowed_actions: Vec<ActionTag>,
}

impl GameContext {
    pub fn from_game(game: &game::Game) -> Self {
        Self {
            allowed_actions: game.allowed_actions(),
        }
    }

    pub fn enabled(&self, action: &ActionTag) -> bool {
        self.allowed_actions.contains(action)
    }

    pub fn disabled(&self, action: &ActionTag) -> bool {
        !self.enabled(action)
    }
}

#[derive(Template)]
#[template(path = "game/index.html")]
pub struct GameTemplate<'a> {
    characters: &'a [GameRole],
    active_role: Option<RoleName>,
    context: GameContext,
    players: &'a [PlayerInfoTemplate<'a>],
    my: &'a PlayerTemplate<'a>,
    misc: MiscTemplate,
    city: CityTemplate<'a>,
    menu: MainTemplate<'a>,
    end: GameEndTemplate<'a>,
}

impl<'a> GameTemplate<'a> {
    pub fn render_with(
        game: &'a Game,
        my_id: Option<&'a str>,
    ) -> axum::response::Result<Html<String>> {
        let myself = get_myself(game, my_id);
        let player_template = PlayerTemplate::from(myself);
        let players: Vec<_> = game
            .players
            .iter()
            .map(|p| PlayerInfoTemplate::from(p, game))
            .collect();
        let MenuTemplate { menu, context } = MenuTemplate::from(game, my_id);
        log::info!("{:#?}", game.active_turn);
        let mut scores = game
            .players
            .iter()
            .map(|p| (p.name.0.borrow(), game.total_score(p)))
            .collect::<Vec<_>>();
        scores.sort_by_key(|(_, score)| -(*score as isize));

        GameTemplate {
            menu,
            context,
            active_role: game.active_role().ok().map(|role| role.role),
            characters: &game.characters,
            city: CityTemplate::from(
                game,
                myself
                    .map(|p| p.index)
                    .or(game.active_player_index().ok())
                    .unwrap_or(PlayerIndex(0)),
                my_id,
            ),
            misc: MiscTemplate {
                round: game.round,
                deck: game.deck.size(),
                timer: None,
            },
            players: &players,
            my: player_template.borrow(),
            end: GameEndTemplate {
                hidden: game.active_turn != Turn::GameOver,
                players: scores,
            },
        }
        .to_html()
    }
}

fn get_myself<'a>(game: &'a Game, myself: Option<&'a str>) -> Option<&'a game::Player> {
    if cfg!(feature = "dev") {
        game.active_player().ok()
    } else if let Some(id) = myself {
        game.players.iter().find(|p| p.id == id)
    } else {
        None
    }
}

#[derive(Template)]
#[template(path = "game/menu.html")]
pub struct MenuTemplate<'a> {
    menu: MainTemplate<'a>,
    context: GameContext,
}
impl<'a> MenuTemplate<'a> {
    pub fn from(game: &'a game::Game, my_id: Option<&'a str>) -> Self {
        let myself = get_myself(game, my_id);
        let my_turn =
            myself.is_some_and(|p1| game.active_player().is_ok_and(|p2| p1.index == p2.index));

        let header = if !my_turn {
            format!(
                "{}'s Turn",
                game.active_player().map_or("nobody", |p| &p.name.0)
            )
            .into()
        } else if game.active_turn.draft().is_some() {
            ("Draft").into()
        } else if let Ok(c) = game.active_role() {
            (format!("{}'s Turn", c.role.display_name())).into()
        } else {
            ("Game over").into()
        };

        Self {
            context: GameContext {
                allowed_actions: game.allowed_actions(),
            },
            menu: MainTemplate {
                header,
                view: MenuView::from(game, myself),
            },
        }
    }
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
    district_tooltip_class: Cow<'a, str>,
    columns: Vec<Vec<DistrictTemplate>>,
    margin_bottom: f64,
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
    pub active: bool,
    pub name: &'a str,
    pub gold: usize,
    pub hand_size: usize,
    pub city_size: usize,
    pub score: usize,
    pub crowned: bool,
    pub complete_city: bool,
    pub first_complete_city: bool,
    pub roles: Vec<(bool, Cow<'a, str>)>,
}

impl<'a> PlayerInfoTemplate<'a> {
    pub fn from(player: &'a game::Player, game: &'a Game) -> Self {
        let count = player.roles.len();
        let mut roles = Vec::with_capacity(count);
        for role in player.roles.iter() {
            if game.characters[role.rank().to_index()].revealed {
                roles.push((
                    game.active_role().is_ok_and(|c| c.role == *role),
                    format!("{role:?}").into(),
                ));
            }
        }
        for _ in roles.len()..count {
            roles.push((false, "?".into()));
        }

        Self {
            active: game.active_player_index().is_ok_and(|i| i == player.index),
            name: player.name.0.borrow(),
            gold: player.gold,
            hand_size: player.hand.len(),
            city_size: player.city.len(),
            crowned: game.crowned == player.index,
            first_complete_city: game
                .first_to_complete
                .as_ref()
                .is_some_and(|c| *c == player.index),
            complete_city: player.city.len() >= game.complete_city_size(),
            score: game.public_score(player),
            roles,
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

#[derive(Clone)]
pub struct DistrictTemplate {
    pub name: &'static str,
    pub cost: usize,
    pub value: String,
    pub suit: CardSuit,
    pub description: Option<&'static str>,
    pub beautified: bool,
    pub asset: ImageAssetTemplate,
    pub pos: Position,
}

#[derive(Clone, Default)]
pub struct Position {
    pub x: f64,
    pub y: f64,
    pub z: isize,
}

impl DistrictTemplate {
    pub fn from(district: DistrictName) -> Self {
        let data = district.data();
        let length = 170.0;
        let scale = 10.0;
        let (brightness, p_x, p_y) = Self::customize(district);
        let offset_x = p_x * length;
        let offset_y = p_y * length;

        let full_height = length * scale / 5.0;
        let full_width = length * (125.8 / 200.0) * (scale / 5.0);
        Self {
            name: data.display_name,
            cost: data.cost,
            value: format!("{:#?}", district),
            suit: data.suit,
            description: data.description,
            beautified: false,
            pos: Position::default(),
            asset: ImageAssetTemplate {
                brightness,
                path: "/public/districts.jpeg",
                height: length,
                width: length,
                scale_percentage: scale * 100.0,
                offset_x: -offset_x + -full_width * (district as usize % 10) as f64,
                offset_y: -offset_y + -full_height * (district as usize / 10) as f64,
            },
        }
    }

    pub fn from_city(player_name: &str, index: usize, district: &CityDistrict) -> Self {
        let mut template = Self::from(district.name);
        template.beautified = district.beautified;
        template.pos.y = -185.0 * index as f64;
        template.value = format!("{},{},{}", template.value, player_name, district.beautified);
        template
    }

    // brightness, x, y
    fn customize(district: DistrictName) -> (f64, f64, f64) {
        match district {
            // yellow
            DistrictName::Manor => (1.3, 0.236, 0.2),
            DistrictName::Palace => (1.3, 0.236, 0.05),

            // blue
            DistrictName::Temple => (1.3, 0.236, 0.3),
            DistrictName::Church => (1.3, 0.236, 0.4),
            DistrictName::Monastery => (1.3, 0.236, 0.7),
            DistrictName::Cathedral => (1.5, 0.236, 0.7),

            // green
            DistrictName::Market => (1.3, 0.236, 0.7),
            DistrictName::Tavern => (1.3, 0.236, 0.4),
            DistrictName::TradingPost => (1.3, 0.236, 0.7),
            DistrictName::Docks => (1.5, 0.236, 0.5),
            DistrictName::Harbor => (1.3, 0.236, 0.7),
            DistrictName::TownHall => (1.3, 0.236, 0.6),

            // red
            DistrictName::Prison => (1.5, 0.236, 0.3),
            DistrictName::Baracks => (1.3, 0.236, 0.3),
            DistrictName::Fortress => (1.5, 0.236, 0.15),

            DistrictName::Library => (1.0, 0.27, 0.3),
            DistrictName::GoldMine => (1.5, 0.236, 0.3),
            DistrictName::Statue => (1.3, 0.236, 0.0),
            DistrictName::SchoolOfMagic => (1.5, 0.236, 0.3),
            DistrictName::ImperialTreasury => (1.5, 0.236, 0.3),
            DistrictName::Observatory => (2.0, 0.236, 0.12),
            DistrictName::MapRoom => (1.5, 0.236, 0.4),
            DistrictName::DragonGate => (1.5, 0.236, 0.4),
            DistrictName::SecretVault => (1.3, 0.236, 0.15),
            DistrictName::Quarry => (1.3, 0.236, 0.5),
            DistrictName::HauntedQuarter => (1.3, 0.236, 0.4),
            DistrictName::GreatWall => (1.3, 0.236, 0.2),
            DistrictName::WishingWell => (2.0, 0.236, 0.1),
            DistrictName::Park => (1.2, 0.25, 0.0),
            DistrictName::Museum => (1.2, 0.27, 0.1),
            DistrictName::IvoryTower => (1.3, 0.236, 0.1),
            _ => (1.3, 0.236, 0.0),
        }
    }
}

pub enum MenuView {
    Logs {
        logs: Vec<Cow<'static, str>>,
    },
    Draft {
        roles: Vec<RoleTemplate>,
        discard: Vec<RoleTemplate>,
        actions: Vec<ActionTag>,
    },
    Call {
        abilities: Vec<ActionTag>,
    },
    Followup {
        action: ActionTag,
        revealed: Vec<DistrictTemplate>,
    },
}

impl MenuView {
    pub fn from(game: &Game, myself: Option<&game::Player>) -> Self {
        let my_turn =
            myself.is_some_and(|p1| game.active_player().is_ok_and(|p2| p1.index == p2.index));
        if !my_turn {
            return MenuView::Logs {
                logs: game.active_role().map_or(vec![], |c| c.logs.clone()),
            };
        }
        let allowed = game.allowed_actions();
        let abilities = game
            .allowed_actions()
            .iter()
            .cloned()
            .filter(|a| {
                !a.is_resource_gathering() && *a != ActionTag::Build && *a != ActionTag::EndTurn
            })
            .collect();

        log::info!("{:#?}", game.active_turn);
        match game.active_turn {
            Turn::GameOver => MenuView::Call { abilities },
            Turn::Draft(_) => MenuView::Draft {
                actions: allowed,
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

                None => MenuView::Call { abilities },
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
    pub rank: Rank,
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
                brightness: 1.0,
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

#[derive(Template)]
#[template(path = "game/end.html")]
pub struct GameEndRootTemplate<'a> {
    pub end: GameEndTemplate<'a>,
}

pub struct GameEndTemplate<'a> {
    pub hidden: bool,
    pub players: Vec<(&'a str, usize)>,
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
