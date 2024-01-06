use crate::{districts::DistrictName, game::PlayerName, roles::RoleName};
use macros::tag::Tag;
use serde::Deserialize;

#[derive(Deserialize, Tag, Debug)]
#[serde(tag = "action")]
pub enum Action {
    // Draft Phase
    DraftPick { role: RoleName },
    DraftDiscard { role: RoleName },

    // Call phase actions
    // Gain resource step
    ResourceGainGold,
    ResourceGainCards,
    ResourcePickCards { district: Vec<DistrictName> },

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
    Steal,
    MagicianSwap(MagicianAction),
    MerchantGainOneGold,
    ArchitectGainCards,
    WarlordDestroy,
    ArtistBeautify,
}

impl ActionTag {
    pub fn is_resource_action(self) -> bool {
        match self {
            Self::ResourceGainGold => true,
            Self::ResourceGainCards => true,
            _ => false,
        }
    }
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum MagicianAction {
    TargetPlayer { player: PlayerName },
    TargetDeck { discard: Vec<DistrictName> },
}
