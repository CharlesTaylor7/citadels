use crate::{
    game::Game,
    types::{Character, RoleName},
};
use serde::Deserialize;

#[derive(Deserialize, Debug)]
#[serde(tag = "action")]
pub enum Action {
    // Draft Phase
    Draft { role: RoleName },
    // Role Call
    GainGold,
    GainCard,
    Build { district: String },
    //    character actions
    //    Assassinate,
    //    ThiefSteal,
    //    MagicianSwapWithPlayer,
    //    // two parts, but maybe some hyperscript?
    //    // We don't need to round trip if they select deck. They just pick the cards.
    //    // ooh, we could
    //    MagicianSwapWithDeck,
    //    KingGainGold,
    //    BishopGain,
    //    MerchantGainOne,
    //    MerchantGainGold,
    //    // architect enables extra builds, but doesn't have any optional activated abilities
    //    WarlordDestroy,
    //    WarlordGainGold,
    //    ArtistBeautify,
}

use std;
pub struct Error {
    pub msg: String,
}
pub type Result = std::result::Result<(), Error>;

impl Action {
    pub fn perform(self, game: &mut Game) -> Result {
        match self {
            Action::Draft { role } => {
                todo!()
            }
            _ => {
                todo!("action {:#?} is not implemented", self);
            }
        }
    }
}
