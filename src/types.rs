use std::fmt::{self, Debug};



#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CardSet {
    Base,
    DarkCity,
    Citadels2016,
    Custom,
}
