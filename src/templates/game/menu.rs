use super::{get_myself, GameContext};
use crate::actions::ActionTag;
use crate::game::{Call, Draft, Followup, ForcedToGatherReason, Game, Player, Turn};
use crate::roles::{Rank, RoleName};
use crate::templates::filters;
use crate::templates::game::menus::{BuildMenu, EmperorMenu};
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
    Bewitch {
        roles: Vec<RoleTemplate>,
    },
    Wizard {
        player: &'a str,
        hand: Vec<DistrictTemplate<'a>>,
        build: BuildMenu,
    },
    Emperor {
        players: Vec<&'a str>,
    },
    Theater {
        players: Vec<&'a str>,
        roles: Vec<RoleTemplate>,
    },
    SeerDistribute {
        players: Vec<&'a str>,
        hand: Vec<DistrictTemplate<'a>>,
    },
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
        logs: &'a [Cow<'static, str>],
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
                Followup::Bewitch => MenuView::Bewitch {
                    roles: game
                        .characters
                        .iter_c()
                        .filter(|c| c.role.rank() > Rank::One)
                        .map(|c| RoleTemplate::from(c.role, 150.0))
                        .collect(),
                },

                Followup::HandleBlackmail { .. } => MenuView::HandleBlackmail {
                    blackmailer: game.players[game.characters.get(Rank::Two).player.unwrap().0]
                        .name
                        .borrow(),
                    bribe: game.active_player().unwrap().gold / 2,
                },
                Followup::Blackmail { .. } => MenuView::RevealBlackmail {
                    gold: game.active_player().unwrap().gold,
                    player: game.active_player().unwrap().name.borrow(),
                    actions: vec![ActionTag::RevealBlackmail, ActionTag::Pass],
                },
                Followup::WizardPick { player } => MenuView::Wizard {
                    build: BuildMenu::from_game(game),
                    player: game.players[player.0].name.borrow(),
                    hand: game.players[player.0]
                        .hand
                        .iter()
                        .map(|d| DistrictTemplate::from(*d))
                        .collect(),
                },
                Followup::SeerDistribute { players } => MenuView::SeerDistribute {
                    hand: game
                        .active_player()
                        .unwrap()
                        .hand
                        .iter()
                        .map(|d| DistrictTemplate::from(*d))
                        .collect(),
                    players: players
                        .iter()
                        .map(|index| game.players[index.0].name.borrow())
                        .collect(),
                },
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
                        .copied()
                        .map(DistrictTemplate::from)
                        .collect(),
                },
                Followup::ScholarPick { revealed } => MenuView::Followup {
                    role: RoleName::Scholar.display_name(),
                    action: ActionTag::ScholarPick,
                    revealed: revealed
                        .iter()
                        .copied()
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

            if let Ok(Call {
                rank: Rank::Four,
                end_of_round: true,
            }) = game.active_turn.call()
            {
                return MenuView::Emperor {
                    players: EmperorMenu::from_game(game).players,
                };
            }

            let allowed = game.active_player_actions();
            let abilities = game
                .active_player_actions()
                .iter()
                .copied()
                .filter(|a| {
                    !a.is_resource_gathering() && *a != ActionTag::Build && *a != ActionTag::EndTurn
                })
                .collect();

            return match game.active_turn.borrow() {
                Turn::GameOver => MenuView::GameOver,
                Turn::Draft(Draft {
                    theater_step: true, ..
                }) => MenuView::Theater {
                    players: game
                        .players
                        .iter()
                        .filter(|p| p.index != myself.unwrap().index)
                        .map(|p| p.name.borrow())
                        .collect(),
                    roles: myself
                        .unwrap()
                        .roles
                        .iter()
                        .map(|r| RoleTemplate::from(*r, 200.0))
                        .collect(),
                },
                Turn::Draft(draft) => MenuView::Draft {
                    actions: allowed,
                    roles: draft
                        .remaining
                        .iter()
                        .map(|r| RoleTemplate::from(*r, 200.0))
                        .collect::<Vec<_>>(),
                    discard: draft
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
                logs: game.active_role().map_or(&[], |c| &c.logs),
            };
        }
    }
}
