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
    pub menu: MainTemplate<'a>,
    pub context: GameContext<'a>,
}
impl<'a> MenuTemplate<'a> {
    pub fn from(game: &'a Game, my_id: Option<&'a str>) -> Self {
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
                game,
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
    pub header: Cow<'a, str>,
    pub view: MenuView<'a>,
}

pub enum MenuView<'a> {
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
