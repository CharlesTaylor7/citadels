use core::slice::SlicePattern;
use std::borrow::Cow;

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

    // character specific actions
    Assassinate,                  // select 1 role
    Steal,                        // select 1 role
    MagicianSwap(MagicianAction), // select 1 player, or select many cards from hand
    MerchantGainOneGold,
    ArchitectGainCards,
    WarlordDestroy, // select a player, then select a city district
    ArtistBeautify, // select one of your district cities
}

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
