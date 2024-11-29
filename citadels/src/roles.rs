use serde::{Deserialize, Serialize};
use std::fmt::Debug;

use crate::{
    actions::ActionTag,
    data::characters::ROLES,
    types::{CardSet, CardSuit},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Rank {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
}

impl Rank {
    pub fn iter() -> impl Iterator<Item = Rank> {
        (0..9_u8).map(|r| unsafe { std::mem::transmute(r) })
    }

    pub fn next(&self) -> Option<Rank> {
        let index = *self as u8 + 1;
        if index <= Rank::Nine as u8 {
            unsafe { std::mem::transmute(index) }
        } else {
            None
        }
    }
    pub fn to_index(&self) -> usize {
        *self as usize
    }
}

impl std::fmt::Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", *self as u8 + 1)
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Deserialize, Serialize, Hash)]
#[repr(usize)]
/// Laid out in the order of the asset file for their images
pub enum RoleName {
    Assassin,
    Witch,
    Magistrate,

    Thief,
    Spy,
    Blackmailer,

    Magician,
    Wizard,
    Seer,

    King,
    Emperor,
    Patrician,

    Bishop,
    Abbot,
    Cardinal,

    Merchant,
    Alchemist,
    Trader,

    Architect,
    Navigator,
    Scholar,

    Warlord,
    Diplomat,
    Marshal,

    Queen,
    Artist,
    TaxCollector,
}

impl RoleName {
    pub fn iter() -> impl Iterator<Item = RoleName> {
        (0..27).map(|i: usize| unsafe { std::mem::transmute(i) })
    }

    pub fn rank(self) -> Rank {
        self.data().rank
    }

    pub fn display_name(self) -> String {
        match self {
            Self::TaxCollector => "Tax Collector".to_owned(),
            _ => format!("{:#?}", self),
        }
    }

    pub fn min_player_count(self) -> usize {
        match self {
            Self::Queen => 5,
            Self::Emperor => 3,
            Self::Artist => 3,
            Self::TaxCollector => 3,
            _ => 0,
        }
    }

    pub fn data(self) -> &'static RoleData {
        &ROLES[self as usize]
    }

    pub fn can_be_discarded_faceup(self) -> bool {
        // rank 4 cards cannot be discarded faceup during the draft.
        // see rulebook page 3
        self.data().rank != Rank::Four
    }

    pub fn build_limit(self) -> usize {
        match self {
            RoleName::Architect => 3,
            RoleName::Navigator => 0,
            RoleName::Scholar => 2,
            RoleName::Seer => 2,
            _ => 1,
        }
    }
}

/// Immutable data
#[derive(Clone)]
pub struct RoleData {
    pub name: RoleName,
    pub rank: Rank,
    pub set: CardSet,
    pub suit: Option<CardSuit>,
    pub description: &'static str,
    pub reminder: &'static str,
    pub actions: &'static [(usize, ActionTag)],
}
