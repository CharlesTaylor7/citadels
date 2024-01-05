use log::info;
use serde::Deserialize;
use std::fmt;

use crate::{data::characters::CHARACTERS, types::Role};

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

impl fmt::Display for RoleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl RoleName {
    pub const fn todo(self) -> Role {
        Role {
            name: self,
            rank: (self as u8) / 3 + 1,
            set: crate::types::CardSet::Custom,
            suit: None,
            description: "todo",
            actions: &[],
        }
    }

    pub fn asset_x(self) -> isize {
        info!(
            "{} {} x:{}, y:{}",
            self,
            self as usize,
            self as usize % 10,
            self as usize / 10
        );
        -155 * (self as isize % 10)
    }

    pub fn asset_y(self) -> isize {
        -265 * (self as isize / 10)
    }

    pub fn data(self) -> &'static Role {
        &CHARACTERS[self as usize]
    }

    pub fn can_be_discarded_faceup(self) -> bool {
        // rank 4 cards cannot be discarded faceup during the draft.
        // see rulebook page 3
        self.data().rank != 4
    }

    pub fn build_limit(self) -> usize {
        match self {
            RoleName::Architect => 3,
            RoleName::Navigator => 0,
            RoleName::Scholar => 2,
            _ => 1,
        }
    }
}
