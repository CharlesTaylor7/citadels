pub struct District {
    pub name: &'static str,
    pub cost: usize,
    pub suit: CardSuit,
    pub set: CardSet,
    pub count: usize,
}

pub struct Character {
    pub name: &'static str,
    pub rank: usize,
    pub set: CardSet,
    pub description: &'static str,
}

#[derive(Debug)]
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

pub enum CardSet {
    Base,
    DarkCity,
    Citadels2016,
}
