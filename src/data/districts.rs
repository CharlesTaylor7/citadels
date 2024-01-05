use crate::types::CardSet::*;

use crate::types::CardSuit::*;
use crate::types::District;
use crate::types::UniqueDistrict::*;
use crate::types::*;

pub const NORMAL: [(usize, District); 17] = [
    // 12
    (3, District::normal(Base, Royal, 5, "Palace")),
    (4, District::normal(Base, Royal, 4, "Castle")),
    (5, District::normal(Base, Royal, 3, "Manor")),
    // 11
    (2, District::normal(Base, Military, 5, "Fortress")),
    (3, District::normal(Base, Military, 3, "Baracks")),
    (3, District::normal(Base, Military, 2, "Prison")),
    (3, District::normal(Base, Military, 1, "Watchtower")),
    // 11
    (2, District::normal(Base, Religious, 5, "Cathedral")),
    (3, District::normal(Base, Religious, 3, "Monastery")),
    (3, District::normal(Base, Religious, 2, "Church")),
    (3, District::normal(Base, Religious, 1, "Temple")),
    // 20
    (2, District::normal(Base, Trade, 5, "Town Hall")),
    (3, District::normal(Base, Trade, 4, "Harbor")),
    (3, District::normal(Base, Trade, 3, "Docks")),
    (4, District::normal(Base, Trade, 2, "Market")),
    (3, District::normal(Base, Trade, 2, "Trading Post")),
    (5, District::normal(Base, Trade, 1, "Tavern")),
];

// TODO: implement 14 of these for a full game
pub const UNIQUE: [District; 14] = [
    District {
        suit: Unique,
        set: Citadels2016,
        display_name: "Secret Vault",
        unique_name: Some(SecretVault),
        cost: 1_000_000,
        description:
            Some("The Secret Vault cannot be built. At the end of the game, reveal the Secret Vault from your hand to score 3 extra points."),
    },

    District {
        suit: Unique,
        set: Base,
        display_name: "Dragon Gate",
        unique_name: Some(DragonGate),
        cost: 6,
        description: Some("At the end of the game score 2 extra points.")
    },

    District {
        suit: Unique,
        set: DarkCity,
        display_name: "Park",
        unique_name: Some(Park),
        cost: 6,
        description: Some("If there are no cards in your hand at the end of your turn, gain 2 cards.")
    },
    District {
        suit: Unique,
        set: DarkCity,
        display_name: "Imperial Treasury",
        unique_name: Some(ImperialTreasury), 
        cost: 5,
        description:Some("At the end of the game, score 1 extra point for each gold in your stash."),
    },
    District {
        suit: Unique,
        set: DarkCity,
        display_name: "Wishing Well",
        unique_name: Some(WishingWell), 
        cost: 5,
        description: Some("At the end of the game, score 1 extra point for each UNIQUE district in your city (including Wishing Well)."),
    },
    District {
        suit: Unique,
        set: DarkCity,
        display_name: "Factory",
        unique_name: Some(Factory),
        cost: 5,
        description: Some("You pay 1 fewer gold to build any other UNIQUE district."),
    },
    District {
        suit: Unique,
        set: DarkCity,
        display_name: "Quarry",
        unique_name: Some(Quarry),
        cost: 5,
        description: Some("You can build districts that are identical to districts in your city."),
    },
    District {
        suit: Unique,
        set: DarkCity,
        display_name: "Map Room",
        unique_name: Some(MapRoom),
        cost: 5,
        description: Some("At the end of the game, score 1 extra point for each card in your hand."),
    },
    District {
        suit: Unique,
        set: Base,
        display_name: "School of Magic",
        unique_name: Some(SchoolOfMagic),
        cost: 6,
        description: Some("For abilities that gain resources for your districts, the School of Magic counts as the district type of your choice."),
    },

    District {
        suit: Unique,
        set: Base,
        display_name: "Haunted Quarter",
        unique_name: Some(HauntedQuarter),
        cost: 2,
        description: Some("At the end of the game, the Haunted Quarter counts as any 1 district type of your choice."),
    },

    District {
        suit: Unique,
        set: Base,
        display_name: "Keep",
        unique_name: Some(Keep),
        cost: 6,
        description: Some("The rank 8 character (Warlord/Diplomat/Marshal) cannot use its ability on the Keep."),
    },

    District {
        suit: Unique,
        set: Base,
        display_name: "Smithy",
        unique_name: Some(Smithy),
        cost: 5,
        description:Some("Once per turn, pay 2 gold to gain 3 cards."),
    },

    District {
        suit: Unique,
        set: Citadels2016,
        display_name: "Statue",
        unique_name: Some(Statue),
        cost: 3,
        description: Some("If you have the crown at the end of the game, score 5 extra points.")
    },
    District {
        suit: Unique,
        set: Citadels2016,
        display_name: "Gold Mine",
        unique_name: Some(GoldMine),
        cost: 6,
        description: Some("If you choose to gain gold when gathering resources, gain 1 extra gold.")
    },
];
/*
    },
    District {
        count: 1,
        suit: Unique,
        set: Base,
        display_name: "Library",
        cost: 6,
    },
    District {
        count: 1,
        suit: Unique,
        set: Base,
        display_name: "Observatory",
        cost: 4,
    },
    District {
        count: 1,
        suit: Unique,
        set: Base,
        display_name: "Great Wall",
        cost: 6,
    },
    District {
        count: 1,
        suit: Unique,
        set: Base,
        display_name: "Laboratory",
        cost: 5,
    },
    District {
        count: 1,
        suit: Unique,
        set: DarkCity,
        display_name: "Armory",
        cost: 3,
    },
    District {
        count: 1,
        suit: Unique,
        set: DarkCity,
        display_name: "Poor House",
        cost: 4,
    },
    District {
        count: 1,
        suit: Unique,
        set: DarkCity,
        display_name: "Mueseum",
        cost: 4,
    },
    },
    District ,
    District {
        count: 1,
        suit: Unique,
        set: Citadels2016,
        display_name: "Necropolis",
        cost: 5,
    },
    District {
        count: 1,
        suit: Unique,
        set: Citadels2016,
        display_name: "Stables",
        cost: 2,
    },
    District {
        count: 1,
        suit: Unique,
        set: Citadels2016,
        display_name: "Monument",
        cost: 4,
    },
    District {
        count: 1,
        suit: Unique,
        set: Citadels2016,
        display_name: "Theater",
        cost: 6,
    },
    District {
        count: 1,
        suit: Unique,
        set: Citadels2016,
        display_name: "Thieves' Den",
        cost: 6,
    },
    District {
        count: 1,
        suit: Unique,
        set: Citadels2016,
        display_name: "Ivory Tower",
        cost: 5,
    },
    District {
        count: 1,
        suit: Unique,
        set: Citadels2016,
        display_name: "Basilica",
        cost: 4,
    },
    District {
        count: 1,
        suit: Unique,
        set: Citadels2016,
        display_name: "Capitol",
        cost: 5,
    },
    District {
        count: 1,
        suit: Unique,
        set: Citadels2016,
        display_name: "Framework",
        cost: 3,
    },

    ];

, ("Framework", "You can build a district by destroying the Framework instead of paying that district's cost.")
, ("Basilica", "At the end of the game, score 1 extra point for each district in your city with an odd-numbered cost.")
, ("Ivory Tower", "If the Ivory Tower is the only UNIQUE district in your city at the end of the game, score 5 extra points")
, ("Thieves' Den", "Pay some or all of the Thieves' Den cost with cards from your hand instead of gold at a rate of 1 card to 1 gold.")
, ("Theater", "At the end of each selection phase, you may exchange your chosen  character card with an opponent's character card.")
, ("Stables", "Building the Stables does not count toward your building limit for the turn.")
, ("Necropolis", "You can build the Necropolis by destroying 1 district in your city instead of paying the Necropolis' cost.")
, ("Poor House", "If you have no gold in your stash at the end of your turn, gain 1 gold.")
, ("Armory", "During your turn, destroy the Armory to destroy 1 district of your choice.")
, ("Monument", "You cannot build the Monument if you have 5 or more districts in your city. Treat the Monument as being 2 districts toward your completed city.")
, ("Museum", "Once per turn, assign 1 card from your hand facedown under the Museum. At the end of the game, score 1 extra point for each card under the Museum.")
, ("Observatory", "If you choose to draw cards when gathering resources, draw 3 cards instead of 2.")
, ("Great Wall", "The rank 8 character (Warlord/Diplomat/Marshal) must pay 1 more gold to use its ability on any district in your city.")
, ("Library", "If you choose to draw cards when gathering resources, keep all drawn cards.")
, ("Laboratory", "Once per turn, discard 1 card from your hand to gain 2 gold.")
]
*/
