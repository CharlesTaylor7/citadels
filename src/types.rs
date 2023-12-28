pub struct District {
    name: String,
    cost: usize,
    suit: CardSuit,
    set: CardSet,
    count: usize,
}

pub struct Character {
    name: String,
    rank: usize,
    set: CardSet,
    description: String,
}

#[derive(Debug)]
pub enum CardSuit {
    Military,  // Red
    Trade,     // Green
    Religious, // Blue
    Noble,     // Yellow
    Unique,    // Purple
}

pub enum CardSet {
    Base,
    DarkCity,
    Citadels2016,
}
