use rand::seq::SliceRandom;
use rand_core::RngCore;
use serde::Deserialize;
use std::fmt::Debug;

use crate::{
    actions::ActionTag,
    data::characters::ROLES,
    types::{CardSet, CardSuit},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(PartialEq, Eq, Copy, Clone, Debug, Deserialize)]
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

    pub fn enabled(self) -> bool {
        match self {
            Self::Assassin => true,
            Self::Thief => true,
            Self::Magician => true,
            Self::King => true,
            Self::Bishop => true,
            Self::Merchant => true,
            Self::Architect => true,
            Self::Warlord => true,
            Self::Artist => true,
            // Bonus
            // 4
            Self::Patrician => true,
            // 7
            Self::Scholar => false,
            Self::Navigator => false,
            // everything else
            _ => false,
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

pub fn select<T: RngCore>(rng: &mut T, num_players: usize) -> impl Iterator<Item = RoleName> + '_ {
    // 9th rank is disallowed for 2
    // 9th rank is required for 3
    // 9th rank is optional for 4-7
    // 9th rank is required for 8
    let n = if num_players == 2 { 8 } else { 9 };
    log::info!("Selecting {} roles for {} player game", n, num_players);
    let mut grouped_by_rank = vec![Vec::with_capacity(3); n];

    for r in crate::roles::ROLES {
        log::info!(
            "{} has rank {} with index {}",
            r.name.display_name(),
            r.rank,
            r.rank as u8
        );
        if r.name.enabled() && num_players >= r.name.min_player_count() {
            grouped_by_rank[r.rank.to_index()].push(r.name)
        }
    }

    grouped_by_rank
        .into_iter()
        .filter_map(|roles| roles.choose(rng).cloned())
}
