use super::{get_myself, GameContext};
use crate::actions::ActionTag;
use crate::game::{Followup, ForcedToGatherReason, Game, Player, Turn};
use crate::roles::RoleName;
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
                allowed: game.allowed_for(my_id),
            },
            menu: MenuView::from(game, myself),
        }
    }
}

pub enum MenuView<'a> {
    TODO,
    HandleBlackmail {
        blackmailer: &'a str,
        bribe: usize,
    },
    ForcedGatherResources {
        explanation: Cow<'a, str>,
        role: String,
    },
    Spy {
        player: &'a str,
        hand: Vec<DistrictTemplate<'a>>,
    },
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

        if my_response {
            let o = game.followup.as_ref().unwrap();
            return match o {
                Followup::Blackmail { .. } => MenuView::RevealBlackmail {
                    gold: game.active_player().unwrap().gold,
                    player: game.active_player().unwrap().name.borrow(),
                    actions: vec![ActionTag::RevealBlackmail, ActionTag::Pass],
                },

                Followup::WizardPick { .. } => MenuView::TODO,
                Followup::SeerDistribute { .. } => MenuView::TODO,
                Followup::SpyAcknowledge { player, revealed } => MenuView::Spy {
                    player: player.borrow(),
                    hand: revealed
                        .iter()
                        .map(|d| DistrictTemplate::from(*d))
                        .collect(),
                },
                Followup::Warrant {
                    gold,
                    district,
                    signed,
                    ..
                } => MenuView::RevealWarrant {
                    gold: *gold,
                    player: game.active_player().unwrap().name.borrow(),
                    district: DistrictTemplate::from(*district),
                    actions: if *signed {
                        vec![ActionTag::RevealWarrant, ActionTag::Pass]
                    } else {
                        vec![ActionTag::Pass]
                    },
                },

                Followup::GatherCardsPick { revealed } => MenuView::Followup {
                    role: game.active_role().unwrap().role.display_name(),
                    action: ActionTag::GatherCardsPick,
                    revealed: revealed
                        .iter()
                        .cloned()
                        .map(DistrictTemplate::from)
                        .collect(),
                },
                Followup::ScholarPick { revealed } => MenuView::Followup {
                    role: RoleName::Scholar.display_name(),
                    action: ActionTag::ScholarPick,
                    revealed: revealed
                        .iter()
                        .cloned()
                        .map(DistrictTemplate::from)
                        .collect(),
                },
            };
        } else if my_turn {
            if let Some(reason) = game.forced_to_gather_resources() {
                return MenuView::ForcedGatherResources {
                    role: game.active_role().unwrap().role.display_name(),
                    explanation: match reason {
                        ForcedToGatherReason::Witch => "Your turn ends after you gather resources. Your turn may resume as the bewitched player."
                            .into(),
                        ForcedToGatherReason::Bewitched => "You have been bewitched! Your turn ends after gathering resources.".into(),
                        ForcedToGatherReason::Blackmailed => "You have been blackmailed! You must gather resources now, and then decide how to handle the blackmail.".into(),
                    },
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

            return match game.active_turn {
                Turn::GameOver => MenuView::GameOver,
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

                Turn::Call(_) => MenuView::Call {
                    role: game.active_role().unwrap().role.display_name(),
                    abilities,
                },
            };
        } else {
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
    }
}
