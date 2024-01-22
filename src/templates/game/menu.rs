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
    pub fn from(game: &'a Game, my_id: Option<Cow<'a, str>>) -> Self {
        let myself = get_myself(game, my_id);
        Self {
            context: GameContext {
                game,
                allowed: game.allowed_for(my_id),
            },
            menu: MenuView::from(game, myself),
        }
    }
}

pub enum MenuView<'a> {
    GameOver,
    Logs {
        header: Cow<'a, str>,
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
            myself.is_some_and(|p1| game.active_player_index().is_ok_and(|p2| p1.index == p2));

        let my_response = myself.is_some_and(|p1| {
            game.responding_player_index()
                .is_ok_and(|p2| p1.index == p2)
        });

        if !my_turn && !my_response {
            return MenuView::Logs {
                header: match game.active_turn {
                    Turn::Draft { .. } => {
                        format!("Draft: {}", game.active_player().unwrap().name).into()
                    }
                    Turn::Call { .. } => format!(
                        "{} ({})",
                        game.active_role().unwrap().role.display_name(),
                        game.active_player().unwrap().name
                    )
                    .into(),
                    Turn::GameOver { .. } => format!("Game over").into(),
                },
                logs: game.active_role().map_or(vec![], |c| c.logs.clone()),
            };
        }
        let allowed = game.active_player_actions();
        let abilities = game
            .active_player_actions()
            .iter()
            .cloned()
            .filter(|a| {
                !a.is_resource_gathering() && *a != ActionTag::Build && *a != ActionTag::EndTurn
            })
            .collect();

        log::info!("{:#?}", game.active_turn);

        if let Some(o) = game.pause_for_response.as_ref() {
            return match o {
                ResponseAction::Blackmail { blackmailer } => MenuView::RevealBlackmail {
                    gold: 777,
                    player: "TODO",
                    actions: abilities,
                },
                ResponseAction::Warrant {
                    magistrate,
                    gold,
                    district,
                } => MenuView::RevealWarrant {
                    gold: *gold,
                    player: game.active_player().unwrap().name.borrow(),
                    district: DistrictTemplate::from(*district),
                    actions: if game.can_reveal_warrant() {
                        vec![ActionTag::RevealWarrant, ActionTag::Pass]
                    } else {
                        vec![ActionTag::Pass]
                    },
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
