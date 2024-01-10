use serde::Deserialize;
use std::fmt::{self, Debug, Display};

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
    Killed,
    Bewitched,
    Robbed,
    Threat { flowered: bool },
    Warrant { signed: bool },
}

pub type PlayerId = String;
pub type Result<T> = std::result::Result<T, &'static str>;

#[derive(Default, Debug, PartialEq, Eq, Clone, Deserialize)]
pub struct PlayerName(pub String);

impl PlayerName {
    pub fn from(str: String) -> Self {
        PlayerName(str)
    }
}

impl Display for PlayerName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq<PlayerName> for &PlayerName {
    fn eq(&self, other: &PlayerName) -> bool {
        self.0.eq(&other.0)
    }
}
