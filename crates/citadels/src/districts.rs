use crate::actions::ActionTag;
use crate::types::CardSet;
use crate::types::CardSet::*;
use crate::types::CardSuit;
use crate::types::CardSuit::*;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
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

// Immutable data
#[derive(Clone, Debug)]
pub struct DistrictData {
    pub name: DistrictName,
    pub display_name: &'static str,
    pub cost: usize,
    pub suit: CardSuit,
    pub set: CardSet,
    pub description: Option<&'static str>,
}

impl DistrictName {
    pub const fn normal(
        self,
        set: CardSet,
        suit: CardSuit,
        cost: usize,
        display_name: &'static str,
    ) -> DistrictData {
        DistrictData {
            name: self,
            set,
            suit,
            cost,
            display_name,
            description: None,
        }
    }

    pub const fn unique(
        self,
        set: CardSet,
        display_name: &'static str,
        cost: usize,
        description: &'static str,
    ) -> DistrictData {
        DistrictData {
            name: self,
            set,
            suit: Unique,
            cost,
            display_name,
            description: Some(description),
        }
    }

    pub fn data(self) -> &'static DistrictData {
        let i = self as usize;
        let n = NORMAL.len();
        if i < n {
            &NORMAL[i]
        } else {
            &UNIQUE[i - n]
        }
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

    pub fn multiplicity(self) -> usize {
        match self {
            Self::Palace => 3,
            Self::Castle => 4,
            Self::Manor => 5,

            Self::Fortress => 2,
            Self::Baracks => 3,
            Self::Prison => 3,
            Self::Watchtower => 3,

            Self::Cathedral => 2,
            Self::Monastery => 3,
            Self::Church => 3,
            Self::Temple => 3,

            Self::TownHall => 2,
            Self::Harbor => 3,
            Self::Docks => 3,
            Self::Market => 4,
            Self::TradingPost => 3,
            Self::Tavern => 5,
            _ => {
                assert!(self as usize >= NORMAL.len());
                1
            }
        }
    }
}

pub const NORMAL: [DistrictData; 17] = [
    DistrictName::Temple.normal(Base, Religious, 1, "Temple"),
    DistrictName::Church.normal(Base, Religious, 2, "Church"),
    DistrictName::Monastery.normal(Base, Religious, 3, "Monastery"),
    DistrictName::Cathedral.normal(Base, Religious, 5, "Cathedral"),
    DistrictName::Watchtower.normal(Base, Military, 1, "Watchtower"),
    DistrictName::Prison.normal(Base, Military, 2, "Prison"),
    DistrictName::Baracks.normal(Base, Military, 3, "Baracks"),
    DistrictName::Fortress.normal(Base, Military, 5, "Fortress"),
    DistrictName::Manor.normal(Base, Noble, 3, "Manor"),
    DistrictName::Castle.normal(Base, Noble, 4, "Castle"),
    DistrictName::Palace.normal(Base, Noble, 5, "Palace"),
    DistrictName::Tavern.normal(Base, Trade, 1, "Tavern"),
    DistrictName::Market.normal(Base, Trade, 2, "Market"),
    DistrictName::TradingPost.normal(Base, Trade, 2, "Trading Post"),
    DistrictName::Docks.normal(Base, Trade, 3, "Docks"),
    DistrictName::Harbor.normal(Base, Trade, 4, "Harbor"),
    DistrictName::TownHall.normal(Base, Trade, 5, "Town Hall"),
];

pub const UNIQUE: [DistrictData; 30] = [
    DistrictData {
        name: DistrictName::Smithy,
        suit: Unique,
        set: Base,
        display_name: "Smithy",
        cost: 5,
        description:Some("Once per turn, pay 2 gold to gain 3 cards."),
    },
    DistrictData {
        name: DistrictName::Laboratory,
        suit: Unique,
        set: Base,
        display_name: "Laboratory",
        cost: 5,
        description: Some("Once per turn, discard 1 card from your hand to gain 2 gold."),
    },
    DistrictData {
        name: DistrictName::SchoolOfMagic,
        suit: Unique,
        set: Base,
        display_name: "School of Magic",
        cost: 6,
        description: Some("For abilities that gain resources for your districts, the School of Magic counts as the district type of your choice."),
    },
    DistrictData {
        name: DistrictName::Keep,
        suit: Unique,
        set: Base,
        display_name: "Keep",
        cost: 3,
        description: Some("The rank 8 character cannot use its ability on the Keep."),
    },
    DistrictData {
        name: DistrictName::DragonGate,
        suit: Unique,
        set: Base,
        display_name: "Dragon Gate",
        cost: 6,
        description: Some("At the end of the game score 2 extra points.")
    },
    DistrictData {
        name: DistrictName::HauntedQuarter,
        suit: Unique,
        set: Base,
        display_name: "Haunted Quarter",
        cost: 2,
        description: Some("At the end of the game, the Haunted Quarter counts as any 1 district type of your choice."),
    },
    DistrictData {
        name: DistrictName::GreatWall,
        suit: Unique,
        set: Base,
        display_name: "Great Wall",
        cost: 6,
        description: Some("The rank 8 character must pay 1 more gold to use its ability on any district in your city."),
    },
    DistrictData {
        name: DistrictName::Observatory,
        suit: Unique,
        set: Base,
        display_name: "Observatory",
        cost: 4,
        description: Some("If you choose to draw cards when gathering resources, draw 3 cards instead of 2."),
    },
    DistrictData {
        name: DistrictName::Library,
        suit: Unique,
        set: Base,
        display_name: "Library",
        cost: 6,
        description: Some("If you choose to draw cards when gathering resources, keep all drawn cards."),
    },
    DistrictData {
        name: DistrictName::Quarry,
        suit: Unique,
        set: DarkCity,
        display_name: "Quarry",
        cost: 5,
        description: Some("You can build districts that are identical to districts in your city."),
    },
    DistrictData {
        name: DistrictName::Armory,
        suit: Unique,
        set: DarkCity,
        display_name: "Armory",
        cost: 3,
        description: Some( "During your turn, destroy the Armory to destroy 1 district of your choice.")
    },
    DistrictData {
        name: DistrictName::Factory,
        suit: Unique,
        set: DarkCity,
        display_name: "Factory",
        cost: 5,
        description: Some("You pay 1 fewer gold to build any other UNIQUE district."),
    },
    DistrictData {
        name: DistrictName::Park,
        suit: Unique,
        set: DarkCity,
        display_name: "Park",
        cost: 6,
        description: Some("If there are no cards in your hand at the end of your turn, gain 2 cards.")
    },

    DistrictData {
        name: DistrictName::Museum,
        suit: Unique,
        set: DarkCity,
        display_name: "Museum",
        cost: 4,
        description: Some("Once per turn, assign 1 card from your hand facedown under the Museum. At the end of the game, score 1 extra point for each card under the Museum.")
    },
    DistrictData {
        name: DistrictName::PoorHouse,
        suit: Unique,
        set: DarkCity,
        display_name: "Poor House",
        cost: 4,
        description: Some("If you have no gold in your stash at the end of your turn, gain 1 gold.")
    },
    DistrictData {
        name: DistrictName::MapRoom,
        suit: Unique,
        set: DarkCity,
        display_name: "Map Room",
        cost: 5,
        description: Some("At the end of the game, score 1 extra point for each card in your hand."),
    },

    DistrictData {
        name: DistrictName::WishingWell,
        suit: Unique,
        set: DarkCity,
        display_name: "Wishing Well",
        cost: 5,
        description: Some("At the end of the game, score 1 extra point for each UNIQUE district in your city (including Wishing Well)."),
    },
    DistrictData {
        name: DistrictName::ImperialTreasury,
        suit: Unique,
        set: DarkCity,
        display_name: "Imperial Treasury",
        cost: 5,
        description:Some("At the end of the game, score 1 extra point for each gold in your stash."),
    },
    DistrictData {
        name: DistrictName::Framework,
        suit: Unique,
        set: Citadels2016,
        display_name: "Framework",
        cost: 3,
        description: Some("You can build a district by destroying the Framework instead of paying that district's cost."),
    },
    DistrictData {
        name: DistrictName::Statue,
        suit: Unique,
        set: Citadels2016,
        display_name: "Statue",
        cost: 3,
        description: Some("If you have the crown at the end of the game, score 5 extra points.")
    },
    DistrictData {
        name: DistrictName::GoldMine,
        suit: Unique,
        set: Citadels2016,
        display_name: "Gold Mine",
        cost: 6,
        description: Some("If you choose to gain gold when gathering resources, gain 1 extra gold.")
    },
    DistrictData {
        name: DistrictName::IvoryTower,
        suit: Unique,
        set: Citadels2016,
        display_name: "Ivory Tower",
        cost: 5,
        description: Some( "If the Ivory Tower is the only UNIQUE district in your city at the end of the game, score 5 extra points")
    },
    DistrictData {
        name: DistrictName::Necropolis,
        suit: Unique,
        set: Citadels2016,
        display_name: "Necropolis",
        cost: 5,
        description: Some( "You can build the Necropolis by destroying 1 district in your city instead of paying the Necropolis' cost.")
    },

    DistrictData {
        name: DistrictName::ThievesDen,
        suit: Unique,
        set: Citadels2016,
        display_name: "Thieves' Den",
        cost: 6,
        description: Some( "Pay some or all of the Thieves' Den cost with cards from your hand instead of gold at a rate of 1 card to 1 gold.")
    },
    DistrictData {
        name: DistrictName::Theater,
        suit: Unique,
        set: Citadels2016,
        display_name: "Theater",
        cost: 6,
        description: Some("At the end of each selection phase, you may exchange your chosen character card with an opponent's character card.")
    },

    DistrictData {
        name: DistrictName::Stables,
        suit: Unique,
        set: Citadels2016,
        display_name: "Stables",
        cost: 2,
        description: Some("Building the Stables does not count toward your building limit for the turn.")
    },

    DistrictData {
        name: DistrictName::Basilica,
        suit: Unique,
        set: Citadels2016,
        display_name: "Basilica",
        cost: 4,
        description: Some("At the end of the game, score 1 extra point for each district in your city with an odd-numbered cost."),
    },
    DistrictData {
        name: DistrictName::SecretVault,
        suit: Unique,
        set: Citadels2016,
        display_name: "Secret Vault",
        cost: 1_000_000,
        description: Some("The Secret Vault cannot be built. At the end of the game, reveal the Secret Vault from your hand to score 3 extra points."),
    },
    DistrictData {
        name: DistrictName::Capitol,
        suit: Unique,
        set: Citadels2016,
        display_name: "Capitol",
        cost: 5,
        description: Some("If you have at least 3 districts of the same type at the end of the game, score 3 extra points.")
    },
    DistrictData {
        name: DistrictName::Monument,
        suit: Unique,
        set: Citadels2016,
        display_name: "Monument",
        cost: 4,
        description: Some( "You cannot build the Monument if you have 5 or more districts in your city. Treat the Monument as being 2 districts toward your completed city.")
    },
];
