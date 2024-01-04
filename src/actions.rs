use crate::game::Game;
use crate::roles::RoleName;
use macros::tag::Tag;
use serde::Deserialize;

#[derive(Deserialize, Tag, Debug)]
#[serde(tag = "action")]
pub enum Action {
    // Draft Phase
    DraftPick { role: RoleName },
    DraftDiscard { role: RoleName },
    // Role Call
    GainGold,
    GainCards,
    Build { district: String },
    // Happens automatically when no actions are left.
    // Turn can end early if requested
    EndTurn,

    // gold for district suits
    GoldFromNobility,
    GoldFromReligion,
    GoldFromTrade,
    GoldFromMilitary,

    // character specific actions
    Assassinate,
    ThiefSteal,
    MagicianSwap(MagicianAction),
    MerchantGainOne,
    ArchitectGainCards,
    WarlordDestroy,
    ArtistBeautify,
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum MagicianAction {
    TargetPlayer { player: String }, // name
    TargetDeck { discard: Vec<String> },
}
