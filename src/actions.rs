use std::borrow::Cow;

use crate::{districts::DistrictName, game::PlayerName, roles::RoleName};
use macros::tag::Tag;
use serde::Deserialize;

#[derive(Deserialize, Tag, Debug)]
#[serde(tag = "action")]
pub enum Action {
    // single select a role, then click pick or discard
    // Draft Phase
    DraftPick {
        role: RoleName,
    },
    DraftDiscard {
        role: RoleName,
    },

    // Call phase actions
    // Gain resource step
    ResourceGainGold,
    ResourceGainCards,
    // pick 1 usually, unless roles or districts change this
    ResourcePickCards {
        district: Select<DistrictName>,
    },

    Build {
        district: DistrictName,
    },
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

    // the king and patrician always target themselves.
    // this action must happen each round.
    // The game says "at some point during their turn".
    // Since there's no strategy to waiting, it will happen automatically.
    // If bewitched the original player still claims the crown.
    // If the character is killed it happens at the end of the round.
    TakeCrown,

    Assassinate {
        role: RoleName,
    },
    Steal {
        role: RoleName,
    },

    // select 1 player, or select many cards from hand
    MagicianSwap(MagicianAction),

    // may destroy own district
    // may not destroy from any completed city
    // may not destroy from bishop's city
    WarlordDestroy {
        district: DistrictName,
        player: PlayerName,
        beautified: bool,
    },
    // must be one of your district cities
    Beautify {
        district: DistrictName,
    },
    //

    // LATER

    // emperor always targets someone else
    // Similar to the king and patricain.
    // The action is required.
    // If bewitched, the witch assigns the crown.
    // If killed, the action occurs at the end of the round, and no resources are taken.
    EmperorAssignCrown {
        player: PlayerName,
    },

    // Abbot
    ResourcesFromReligious {
        gold: usize,
        cards: usize,
    },

    // Abbot
    TaxRich,

    // Patrician
    CardsFromNobility,

    // Cardinal
    CardsFromReligious,

    // Witch
    Bewitch {
        role: RoleName,
    },

    // Magistrate
    AssignWarrants {
        warrant: [Warrant; 3],
    },

    // Blackmailer
    AssignThreats {
        threat: [Threat; 2],
    },

    // Tax Collector collects from the tax pool
    CollectTaxes,
}

#[derive(Deserialize, Debug)]
pub struct Warrant {
    pub role: RoleName,
    pub signed: bool,
}

#[derive(Deserialize, Debug)]
pub struct Threat {
    pub role: RoleName,
    pub flowered: bool,
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

impl<T: Clone> Select<T> {
    pub fn to_vec(&self) -> Cow<'_, [T]>
    where
        [T]: ToOwned<Owned = Vec<T>>,
    {
        match self {
            Select::Single(item) => Cow::Owned(vec![item.clone()]),
            Select::Many(items) => Cow::Borrowed(items),
        }
    }
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
