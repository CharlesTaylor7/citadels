use std::fmt;

use serde::Deserialize;
use serde_repr::Deserialize_repr;

#[derive(Clone, Debug)]
pub struct District {
    pub display_name: &'static str,
    pub cost: usize,
    pub suit: CardSuit,
    pub set: CardSet,
    pub unique_name: Option<UniqueDistrict>,
    pub description: Option<&'static str>,
}

impl District {
    pub const fn normal(
        set: CardSet,
        suit: CardSuit,
        cost: usize,
        display_name: &'static str,
    ) -> District {
        District {
            set,
            suit,
            cost,
            display_name,
            unique_name: None,
            description: None,
        }
    }
}

pub type Rank = u8;

#[derive(Clone, Debug)]
pub struct Character {
    pub name: RoleName,
    pub rank: Rank,
    pub set: CardSet,
    pub suit: Option<CardSuit>,
    pub description: &'static str,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Deserialize_repr)]
#[repr(u8)]
pub enum RoleName {
    Assassin,
    Thief,
    Magician,
    King,
    Bishop,
    Merchant,
    Architect,
    Warlord,
    Artist,
}

impl fmt::Display for RoleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CardSuit {
    Military,
    Trade,
    Religious,
    Royal,
    Unique,
}

impl fmt::Display for CardSuit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

/*
#[derive(Debug)]
pub enum CardSuit {
    Military,  // Red
    Trade,     // Green
    Religious, // Blue
    Noble,     // Yellow
    Unique,    // Purple
}
*/

#[derive(Debug, Clone, Copy)]
pub enum CardSet {
    Base,
    DarkCity,
    Citadels2016,
    Custom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UniqueDistrict {
    HauntedQuarter,
    SecretVault,
    GoldMine,
    Framework,
    Basilica,
    IvoryTower,
    ThievesDen,
    Theater,
    Monument,
    Stables,
    Statue,
    Necropolis,
    PoorHouse,
    Factory,
    Quarry,
    MapRoom,
    Park,
    ImperialTreasury,
    WishingWell,
    Armory,
    Museum,
    Observatory,
    GreatWall,
    Keep,
    DragonGate,
    Library,
    Smithy,
    Laboratory,
    SchoolOfMagic,
}
