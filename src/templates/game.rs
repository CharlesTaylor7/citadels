pub mod menu;
pub mod menus;

use crate::actions::ActionTag;
use crate::game::{Game, GameRole, Player, PlayerIndex, Turn};
use crate::roles::RoleName;
use crate::templates::filters;
use crate::templates::game::menu::{MainTemplate, MenuTemplate, MenuView};
use crate::templates::MyTemplate;
use crate::templates::{DistrictTemplate, RoleTemplate};
use crate::types::Marker;
use askama::Template;
use axum::response::Html;
use std::borrow::{Borrow, Cow};

#[derive(Template)]
#[template(path = "game/city.html")]
pub struct CityRootTemplate<'a> {
    city: CityTemplate<'a>,
}

impl<'a> CityRootTemplate<'a> {
    pub fn from(game: &'a Game, target: PlayerIndex, my_id: Option<&'a str>) -> Self {
        Self {
            city: CityTemplate::from(game, target, my_id),
        }
    }
}

impl<'a> CityTemplate<'a> {
    pub fn from(game: &'a Game, target: PlayerIndex, my_id: Option<&'a str>) -> Self {
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
pub struct GameContext<'a> {
    game: &'a Game,
    allowed_actions: Vec<ActionTag>,
}

impl<'a> GameContext<'a> {
    pub fn from_game(game: &'a Game) -> Self {
        Self {
            game,
            allowed_actions: game.allowed_actions(),
        }
    }

    pub fn enabled(&self, action: &ActionTag) -> bool {
        self.allowed_actions.contains(action)
    }

    pub fn disabled(&self, action: &ActionTag) -> bool {
        !self.enabled(action)
    }

    pub fn label(&self, action: &'a ActionTag) -> Cow<'a, str> {
        self.game
            .active_player()
            .map_or("".into(), |p| action.label(p))
    }
}

#[derive(Template)]
#[template(path = "game/index.html")]
pub struct GameTemplate<'a> {
    characters: &'a [GameRole],
    context: GameContext<'a>,
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
            characters: &game.characters.0,
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

pub fn get_myself<'a>(game: &'a Game, myself: Option<&'a str>) -> Option<&'a Player> {
    if cfg!(feature = "dev") {
        game.active_player().ok()
    } else if let Some(id) = myself {
        game.players.iter().find(|p| p.id == id)
    } else {
        None
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
#[template(path = "game/logs.html")]
pub struct LogsTemplate<'a> {
    pub logs: &'a [String],
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
    pub fn from(player: Option<&'a Player>) -> Self {
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
    pub fn from(player: &'a Player, game: &'a Game) -> Self {
        let count = player.roles.len();
        let mut roles = Vec::with_capacity(count);
        for role in player.roles.iter() {
            if game.characters.get(role.rank()).revealed {
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
            complete_city: player.city_size() >= game.complete_city_size(),
            score: game.public_score(player),
            roles,
        }
    }
}