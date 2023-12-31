use crate::{
    game::{Game, Player, Turn},
    types::RoleName,
};
use serde::Deserialize;

#[derive(Deserialize, Debug)]
#[serde(tag = "action")]
pub enum Action {
    // Draft Phase
    DraftPick { role: RoleName },
    DraftDiscard { role: RoleName },
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
    pub msg: &'static str,
}

pub type Result = std::result::Result<(), Error>;

impl Action {
    pub fn perform(self, game: &mut Game) -> Option<()> {
        match self {
            Action::DraftPick { role } => {
                let player_id = game.active_turn.draft()?;
                let p = game.players.iter_mut().find(|p| p.id == *player_id)?;

                let i = (0..game.draft.remaining.len())
                    .find(|i| game.draft.remaining[*i].name == role)?;

                let role = game.draft.remaining.remove(i);
                p.roles.push(role);
                Some(())
            }

            Action::DraftDiscard { role } => {
                let i = (0..game.draft.remaining.len())
                    .find(|i| game.draft.remaining[*i].name == role)?;

                game.draft.remaining.remove(i);
                Some(())
            }

            _ => {
                todo!("action is not implemented");
            }
        }
    }
}
