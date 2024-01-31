use crate::actions::ActionTag;
use crate::districts::DistrictName;
use crate::game::{self, Followup, Game, Player};
use crate::roles::RoleName;
use crate::templates::{filters, DistrictTemplate, RoleTemplate};
use crate::types::CardSuit;
use crate::types::Marker;
use askama::Template;

use std::borrow::{Borrow, Cow};

use super::{CityTemplate, GameContext};

// for the thief, assassin
#[derive(Template)]
#[template(path = "game/menus/select-role.html")]
pub struct SelectRoleMenu<'a> {
    pub roles: Vec<RoleTemplate>,
    pub action: ActionTag,
    pub header: Cow<'a, str>,
    pub context: GameContext<'a>,
}

// for building
#[derive(Template)]
#[template(path = "game/menus/build.html")]
pub struct BuildMenu {
    pub wizard: bool,
    pub cardinal: bool,
    pub thieves_den: bool,
    pub framework: bool,
    pub necropolis: bool,
}

impl BuildMenu {
    pub fn from_game(game: &Game) -> Self {
        Self {
            wizard: if let Some(Followup::WizardPick { .. }) = game.followup {
                true
            } else {
                false
            },
            cardinal: game
                .active_role()
                .is_ok_and(|c| c.role == RoleName::Cardinal),
            thieves_den: game
                .active_player()
                .is_ok_and(|p| p.hand.iter().any(|d| *d == DistrictName::ThievesDen)),
            necropolis: game
                .active_player()
                .is_ok_and(|p| p.hand.iter().any(|d| *d == DistrictName::Necropolis)),
            framework: game
                .active_player()
                .is_ok_and(|p| p.city_has(DistrictName::Framework)),
        }
    }
}

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
#[template(path = "game/menus/laboratory.html")]
pub struct LaboratoryMenu {}

#[derive(Template)]
#[template(path = "game/menus/beautify.html")]
pub struct BeautifyMenu;

#[derive(Template)]
#[template(path = "game/menus/navigator.html")]
pub struct NavigatorMenu;

#[derive(Template)]
#[template(path = "game/menus/museum.html")]
pub struct MuseumMenu;

#[derive(Template)]
#[template(path = "game/menus/abbot-collect-resources.html")]
pub struct AbbotCollectResourcesMenu {
    resources: usize,
}

impl AbbotCollectResourcesMenu {
    pub fn from(game: &Game) -> Self {
        Self {
            resources: game
                .active_player()
                .unwrap()
                .count_suit_for_resource_gain(CardSuit::Religious),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/abbot-take-from-rich.html")]
pub struct AbbotTakeFromRichMenu<'a> {
    pub players: Vec<&'a Player>,
}

impl<'a> AbbotTakeFromRichMenu<'a> {
    pub fn from(game: &'a Game) -> Self {
        Self {
            players: game.abbot_take_from_rich_targets(),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/spy.html")]
pub struct SpyMenu<'a> {
    pub players: Vec<&'a Player>,
    pub suits: &'a [CardSuit],
}

impl<'a> SpyMenu<'a> {
    pub fn from(game: &'a Game) -> Self {
        let active = game.active_player_index().unwrap();
        Self {
            players: game.players.iter().filter(|p| p.index != active).collect(),
            suits: CardSuit::ALL.borrow(),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/send-warrants.html")]
pub struct SendWarrantsMenu {
    roles: Vec<RoleTemplate>,
}

impl SendWarrantsMenu {
    pub fn from_game(game: &game::Game) -> Self {
        Self {
            roles: game
                .characters
                .iter()
                .skip(1)
                .map(|r| RoleTemplate::from(r, 160.0))
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/send-blackmail.html")]
pub struct BlackmailMenu {
    roles: Vec<RoleTemplate>,
}
impl BlackmailMenu {
    pub fn from_game(game: &game::Game) -> Self {
        Self {
            roles: game
                .characters
                .iter_c()
                .skip(2)
                .filter(|c| {
                    c.markers
                        .iter()
                        .all(|m| *m != Marker::Bewitched && *m != Marker::Killed)
                })
                .map(|c| RoleTemplate::from(c.role, 160.0))
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/warlord.html")]
pub struct WarlordMenu<'a> {
    pub cities: Vec<CityTemplate<'a>>,
}

impl<'a> WarlordMenu<'a> {
    pub fn from_game(game: &'a game::Game) -> Self {
        Self {
            cities: game
                .players
                .iter()
                .filter(|p| {
                    !game.characters.has_bishop_protection(p.index)
                        && p.city_size() < game.complete_city_size()
                })
                .map(|p| CityTemplate::from(game, p.index, None))
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/armory.html")]
pub struct ArmoryMenu<'a> {
    pub cities: Vec<CityTemplate<'a>>,
}

impl<'a> ArmoryMenu<'a> {
    pub fn from_game(game: &'a game::Game) -> Self {
        Self {
            cities: game
                .players
                .iter()
                .filter(|p| p.city_size() < game.complete_city_size())
                .map(|p| CityTemplate::from(game, p.index, None))
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/marshal.html")]
pub struct MarshalMenu<'a> {
    pub cities: Vec<CityTemplate<'a>>,
}

impl<'a> MarshalMenu<'a> {
    pub fn from_game(game: &'a game::Game) -> Self {
        Self {
            cities: game
                .players
                .iter()
                .filter(|p| {
                    !game.characters.has_bishop_protection(p.index)
                        && p.city_size() < game.complete_city_size()
                        && game.active_player().is_ok_and(|active| active.id != p.id)
                })
                .map(|p| CityTemplate::from(game, p.index, None))
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/diplomat.html")]
pub struct DiplomatMenu<'a> {
    pub cities: Vec<CityTemplate<'a>>,
}

impl<'a> DiplomatMenu<'a> {
    pub fn from_game(game: &'a game::Game) -> Self {
        Self {
            cities: game
                .players
                .iter()
                .filter(|p| {
                    !game.characters.has_bishop_protection(p.index)
                        && p.city_size() < game.complete_city_size()
                        && game.active_player().is_ok_and(|active| active.id != p.id)
                })
                .map(|p| CityTemplate::from(game, p.index, None))
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/cardinal.html")]
pub struct CardinalMenu<'a> {
    pub players: Vec<&'a str>,
    pub hand: Vec<DistrictTemplate<'a>>,
}

#[derive(Template)]
#[template(path = "game/menus/necropolis.html")]
pub struct NecropolisMenu<'a> {
    pub city: CityTemplate<'a>,
}

#[derive(Template)]
#[template(path = "game/menus/thieves_den.html")]
pub struct ThievesDenMenu<'a> {
    pub hand: Vec<DistrictTemplate<'a>>,
}

#[derive(Template)]
#[template(path = "game/menus/emperor.html")]
pub struct EmperorMenu<'a> {
    pub players: Vec<&'a str>,
}

impl<'a> EmperorMenu<'a> {
    pub fn from_game(game: &'a game::Game) -> Self {
        Self {
            players: game
                .players
                .iter()
                .filter(|p| {
                    game.crowned != p.index
                        && game.active_player().is_ok_and(|active| active.id != p.id)
                })
                .map(|p| p.name.borrow())
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Template)]
#[template(path = "game/menus/wizard.html")]
pub struct WizardMenu<'a> {
    pub players: Vec<&'a str>,
}

impl<'a> WizardMenu<'a> {
    pub fn from_game(game: &'a game::Game) -> Self {
        Self {
            players: game
                .players
                .iter()
                .filter(|p| game.active_player().is_ok_and(|active| active.id != p.id))
                .map(|p| p.name.borrow())
                .collect::<Vec<_>>(),
        }
    }
}
