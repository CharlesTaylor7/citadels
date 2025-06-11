use crate::actions::ActionTag;
use crate::types::CardSet;
use crate::types::CardSuit;
use poem_openapi::Enum;
use serde::{Deserialize, Serialize};

use std::sync::LazyLock;
static DATA: LazyLock<Vec<DistrictData>> =
    LazyLock::new(|| serde_json::from_str(include_str!("../../public/districts.json")).unwrap());

// Immutable data
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DistrictData {
    pub id: usize,
    pub name: DistrictName,
    pub display_name: &'static str,
    pub cost: usize,
    pub suit: CardSuit,
    pub set: CardSet,
    pub description: Option<&'static str>,
    pub multiplicity: usize,
}

impl DistrictData {
    pub fn normal() -> &'static [DistrictData] {
        &DATA[0..17]
    }
    pub fn unique() -> &'static [DistrictData] {
        &DATA[17..]
    }
}

impl DistrictName {
    pub fn data(self) -> &'static DistrictData {
        unsafe { DATA.get_unchecked(self as usize) }
    }

    pub fn action(self) -> Option<ActionTag> {
        match self {
            DistrictName::Smithy => Some(ActionTag::Smithy),
            DistrictName::Museum => Some(ActionTag::Museum),
            DistrictName::Laboratory => Some(ActionTag::Laboratory),
            DistrictName::Armory => Some(ActionTag::Armory),
            _ => None,
        }
    }
}

#[derive(Enum, Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[repr(usize)]
pub enum DistrictName {
    Temple,
    Church,
    Monastery,
    Cathedral,

    Watchtower,
    Prison,
    Baracks,
    Fortress,

    Manor,
    Castle,
    Palace,

    Tavern,
    Market,
    TradingPost,
    Docks,
    Harbor,
    TownHall,

    Smithy,
    Laboratory,
    SchoolOfMagic,
    Keep,
    DragonGate,
    HauntedQuarter,
    GreatWall,
    Observatory,
    Library,
    Quarry,
    Armory,
    Factory,
    Park,
    Museum,
    PoorHouse,
    MapRoom,
    WishingWell,
    ImperialTreasury,
    Framework,
    Statue,
    GoldMine,
    IvoryTower,
    Necropolis,
    ThievesDen,
    Theater,
    Stables,
    Basilica,
    SecretVault,
    Capitol,
    Monument,
}
