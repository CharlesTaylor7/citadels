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

#[derive(Clone, Debug)]
pub struct Character {
    pub name: &'static str,
    pub rank: usize,
    pub set: CardSet,
    pub description: &'static str,
}

#[derive(Debug, Clone, Copy)]
pub enum CardSuit {
    Red,
    Green,
    Blue,
    Yellow,
    Purple,
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
}

#[derive(Debug, Clone, Copy)]
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
