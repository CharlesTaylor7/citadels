use crate::actions::ActionTag;


use crate::roles::{RoleName};
use crate::templates::{filters, RoleTemplate};


use crate::{game};
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
                    !game.characters.has_revealed_role(p, RoleName::Bishop)
                        && p.city_size() < game.complete_city_size()
                })
                .map(|p| CityTemplate::from(game, p.index, None))
                .collect::<Vec<_>>(),
        }
    }
}
