use crate::types::CardSet::*;

use crate::types::CardSuit::*;
use crate::types::District;
use crate::types::UniqueDistrict;

pub const NORMAL: [(usize, District); 17] = [
    // 12
    (3, District::normal(Base, Yellow, 5, "Palace")),
    (4, District::normal(Base, Yellow, 4, "Castle")),
    (5, District::normal(Base, Yellow, 3, "Manor")),
    // 11
    (2, District::normal(Base, Red, 5, "Fortress")),
    (3, District::normal(Base, Red, 3, "Baracks")),
    (3, District::normal(Base, Red, 2, "Prison")),
    (3, District::normal(Base, Red, 1, "Watchtower")),
    // 11
    (2, District::normal(Base, Blue, 5, "Cathedral")),
    (3, District::normal(Base, Blue, 3, "Monastery")),
    (3, District::normal(Base, Blue, 2, "Church")),
    (3, District::normal(Base, Blue, 1, "Temple")),
    // 20
    (2, District::normal(Base, Green, 5, "Town Hall")),
    (3, District::normal(Base, Green, 4, "Harbor")),
    (3, District::normal(Base, Green, 3, "Docks")),
    (4, District::normal(Base, Green, 2, "Market")),
    (3, District::normal(Base, Green, 2, "Trading Post")),
    (5, District::normal(Base, Green, 1, "Tavern")),
];

// TODO: implement 14 of these for a full game
pub const UNIQUE: [District; 2] = [
    District {
        suit: Purple,
        set: Citadels2016,
        display_name: "Secret Vault",
        unique_name: Some(UniqueDistrict::SecretVault),
        cost: 1_000_000,
        description:
            Some("The Secret Vault cannot be built. At the end of the game, reveal the Secret Vault from your hand to score 3 extra points."),
    },

    District {
        suit: Purple,
        set: Base,
        display_name: "Dragon Gate",
        unique_name: Some(UniqueDistrict::DragonGate),
        cost: 6,
        description: Some("At the end of the game score 2 extra points.")
    },
];
/*
(District {
        count: 1,
        suit: Purple,
        set: Base
        name: "Haunted Quarter",
        cost: 2,
    }),
    District {
        count: 1,
        suit: Purple,
        set: Base,
        name: "Keep",
        cost: 6,
    },
    District {
        count: 1,
        suit: Purple,
        set: Base,
        name: "Library",
        cost: 6,
    },
    District {
        count: 1,
        suit: Purple,
        set: Base,
        name: "School of Magic",
        cost: 6,
    },
    District {
        count: 1,
        suit: Purple,
        set: Base,
        name: "Observatory",
        cost: 4,
    },
    District {
        count: 1,
        suit: Purple,
        set: Base,
        name: "Great Wall",
        cost: 6,
    },
    District {
        count: 1,
        suit: Purple,
        set: Base,
        name: "Smithy",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: Base,
        name: "Laboratory",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: DarkCity,
        name: "Armory",
        cost: 3,
    },
    District {
        count: 1,
        suit: Purple,
        set: DarkCity,
        name: "Quarry",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: DarkCity,
        name: "Poor House",
        cost: 4,
    },
    District {
        count: 1,
        suit: Purple,
        set: DarkCity,
        name: "Mueseum",
        cost: 4,
    },
    District {
        count: 1,
        suit: Purple,
        set: DarkCity,
        name: "Factory",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: DarkCity,
        name: "Map Room",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: DarkCity,
        name: "Park",
        cost: 6,
    },
    District {
        count: 1,
        suit: Purple,
        set: DarkCity,
        name: "Imperial Treasury",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: DarkCity,
        name: "Wishing Well",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Necropolis",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Statue",
        cost: 3,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Stables",
        cost: 2,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Monument",
        cost: 4,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Theater",
        cost: 6,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Thieves' Den",
        cost: 6,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Ivory Tower",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Basilica",
        cost: 4,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Capitol",
        cost: 5,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Framework",
        cost: 3,
    },
    District {
        count: 1,
        suit: Purple,
        set: Citadels2016,
        name: "Gold Mine",
        cost: 6,
    },

    ];
*/

/*
[ ("Haunted Quarter", "At the end of the game, the Haunted Quarter counts as any 1 district type of your choice.")
, ("Secret Vault", )
, ("Gold Mine", "If you choose to gain gold when gathering resources, gain 1 extra gold.")
, ("Framework", "You can build a district by destroying the Framework instead of paying that district's cost.")
, ("Basilica", "At the end of the game, score 1 extra point for each district in your city with an odd-numbered cost.")
, ("Ivory Tower", "If the Ivory Tower is the only UNIQUE district in your city at the end of the game, score 5 extra points")
, ("Thieves' Den", "Pay some or all of the Thieves' Den cost with cards from your hand instead of gold at a rate of 1 card to 1 gold.")
, ("Theater", "At the end of each selection phase, you may exchange your chosen  character card with an opponent's character card.")
, ("Monument", "You cannot build the Monument if you have 5 or more districts in your city. Treat the Monument as being 2 districts toward your completed city.")
, ("Stables", "Building the Stables does not count toward your building limit for the turn.")
, ("Statue", "If you have the crown at the end of the game, score 5 extra points.")
, ("Necropolis", "You can build the Necropolis by destroying 1 district in your city instead of paying the Necropolis' cost.")
, ("Poor House", "If you have no gold in your stash at the end of your turn, gain 1 gold.")
, ("Factory", "You pay 1 fewer gold to build any other UNIQUE district.")
, ("Quarry", "You can build districts that are identical to districts in your city.")
, ("Map Room", "At the end of the game, score 1 extra point for each card in your hand.")
, ("Park", "If there are no cards in your hand at the end of your turn, gain 2 cards.")
, ("Imperial Treasury", "At the end of the game, score 1 extra point for each gold in your stash.")
, ("Wishing Well", "At the end of the game, score 1 extra point for each UNIQUE district in your city (including Wishing Well).")
, ("Armory", "During your turn, destroy the Armory to destroy 1 district of your choice.")
, ("Museum", "Once per turn, assign 1 card from your hand facedown under the Museum. At the end of the game, score 1 extra point for each card under the Museum.")
, ("Observatory", "If you choose to draw cards when gathering resources, draw 3 cards instead of 2.")
, ("Great Wall", "The rank 8 character (Warlord/Diplomat/Marshal) must pay 1 more gold to use its ability on any district in your city.")
, ("Keep", "The rank 8 character (Warlord/Diplomat/Marshal) cannot use its ability on the Keep.")
, ("Library", "If you choose to draw cards when gathering resources, keep all drawn cards.")
, ("Smithy", "Once per turn, pay 2 gold to gain 3 cards.")
, ("Laboratory", "Once per turn, discard 1 card from your hand to gain 2 gold.")
, ("School of Magic", "For abilities that gain resources for your districts, the School of Magic counts as the district type of your choice.")
]
*/
