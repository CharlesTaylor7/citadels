use serde::Deserialize;
use std::fmt::{self, Debug};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum CardSuit {
    Noble,
    Religious,
    Trade,
    Military,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Marker {
    Assassinated,
    Bewitched,
    Robbed,
    Threat { flowered: bool },
    Warrant { signed: bool },
}
