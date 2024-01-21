use super::{get_myself, GameContext};
use crate::actions::ActionTag;
use crate::game::{FollowupAction, Game, Player, ResponseAction, Turn};
use crate::templates::filters;
use crate::templates::{DistrictTemplate, RoleTemplate};
use askama::Template;
use std::borrow::{Borrow, Cow};

#[derive(Template)]
#[template(path = "game/menu.html")]
pub struct MenuTemplate<'a> {
    pub menu: MenuView<'a>,
    pub context: GameContext<'a>,
}
impl<'a> MenuTemplate<'a> {
    pub fn from(game: &'a Game, my_id: Option<&'a str>) -> Self {
        let myself = get_myself(game, my_id);
        Self {
            context: GameContext {
                game,
                allowed_actions: game.allowed_actions(),
            },
            menu: MenuView::from(game, myself),
        }
    }
}

pub enum MenuView<'a> {
    GameOver,
    Logs {
        player: &'a str,
        logs: Vec<Cow<'static, str>>,
    },
    Draft {
        roles: Vec<RoleTemplate>,
        discard: Vec<RoleTemplate>,
        actions: Vec<ActionTag>,
    },
    Call {
        role: String,
        abilities: Vec<ActionTag>,
    },
    Followup {
        role: String,
        action: ActionTag,
        revealed: Vec<DistrictTemplate<'a>>,
    },
    RevealWarrant {
        gold: usize,
        player: &'a str,
        district: DistrictTemplate<'a>,
        actions: Vec<ActionTag>,
    },
    RevealBlackmail {
        gold: usize,
        player: &'a str,
        actions: Vec<ActionTag>,
    },
}

impl<'a> MenuView<'a> {
    pub fn from(game: &'a Game, myself: Option<&'a Player>) -> Self {
        let my_turn =
            myself.is_some_and(|p1| game.active_player().is_ok_and(|p2| p1.index == p2.index));

        if !my_turn {
            return MenuView::Logs {
                player: game
                    .active_player()
                    .map_or("game over", |p| p.name.borrow()),
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

        if let Some(o) = game.pause_for_response.as_ref() {
            return match o {
                ResponseAction::Blackmail { .. } => MenuView::RevealBlackmail {
                    gold: 777,
                    player: "TODO",
                    actions: abilities,
                },
                ResponseAction::Warrant { gold, district } => MenuView::RevealWarrant {
                    gold: gold.clone(),
                    player: game.players[game.active_role().unwrap().player.unwrap().0]
                        .name
                        .borrow(),
                    district: DistrictTemplate::from(*district),
                    actions: abilities,
                },
            };
        }
        match game.active_turn {
            Turn::GameOver => MenuView::GameOver {},
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
                    role: game.active_role().unwrap().role.display_name(),
                    action: *action,
                    revealed: revealed
                        .iter()
                        .cloned()
                        .map(DistrictTemplate::from)
                        .collect(),
                },

                None => MenuView::Call {
                    role: game.active_role().unwrap().role.display_name(),
                    abilities,
                },
            },
        }
    }
}
