use crate::{districts::DistrictName, game::PlayerName, roles::RoleName};
use macros::tag::Tag;
use serde::Deserialize;

#[derive(Deserialize, Tag, Debug)]
#[serde(tag = "action")]
pub enum Action {
    // single select a role, then click pick or discard
    // Draft Phase
    DraftPick { role: RoleName },
    DraftDiscard { role: RoleName },

    // Call phase actions
    // Gain resource step
    ResourceGainGold,
    ResourceGainCards,
    // pick 1 usually, unless roles or districts change this
    ResourcePickCards { district: Select<DistrictName> },

    Build { district: DistrictName },
    // Happens automatically when no actions are left.
    // Turn can end early if requested
    EndTurn,

    // gold for district suits
    GoldFromNobility,
    GoldFromReligion,
    GoldFromTrade,
    GoldFromMilitary,
    MerchantGainOneGold,
    ArchitectGainCards,

    // Patrician
    CardsFromNobility,

    // the king and patrician always target themselves.
    // this action must happen each round.
    // The game says "at some point during their turn".
    // Since there's no strategy to waiting, it will happen automatically.
    // If bewitched the original player still claims the crown.
    // If the character is killed it happens at the end of the round.
    TakeCrown,

    // emperor always targets someone else
    // Similar to the king and patricain.
    // The action is required.
    // If bewitched, the witch assigns the crown.
    // If killed, the action occurs at the end of the round, and no resources are taken.
    EmperorAssignCrown { player: PlayerName },

    // character specific actions
    Assassinate,                  // select 1 role
    Steal,                        // select 1 role
    MagicianSwap(MagicianAction), // select 1 player, or select many cards from hand
    WarlordDestroy,               // select a player, then select a city district
    ArtistBeautify,               // select one of your district cities
}

// action target domains:
// a role - Assassinate, Steal, single
// other player - Magician, single
// my hand - Magician, many
// my city - Build, beautify, single
// other city - Warlord, single
// revealed - picking cards revealed from somwhere else, such as top of deck or another hand,
// single or many
// draft - picking roles, single
//
// fluidly the ui needs to swap input types from 3 states:
// disabled = ""
// single = "checkbox"
// many = "radio"
//
// depending on what the active action is.
// I should not leak the UI needs into the domain layer.
// Resource pick cards is the only followup action I need at the moment.
// I may need more followup actions for new roles and districts, but at the moment that's the only
// followup needed.
//
// So I need the UI layer to smartly allow:
// - picking an action,
// - that enables selection from the desired zone/domain.
// - and puts in a simple confirm button into the ui.
//
//
//
#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum Select<T> {
    Single(T),
    Many(Vec<T>),
}

impl<T> Select<T> {
    pub fn len(&self) -> usize {
        match self {
            Select::Single(_) => 1,
            Select::Many(items) => items.len(),
        }
    }
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
