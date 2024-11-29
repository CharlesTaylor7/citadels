use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::fmt::{self, Debug, Display};
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Serialize)]
pub enum CardSuit {
    Trade,
    Religious,
    Military,
    Noble,
    Unique,
}

impl CardSuit {
    pub const ALL: [CardSuit; 5] = [
        Self::Trade,
        Self::Religious,
        Self::Military,
        Self::Noble,
        Self::Unique,
    ];
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
    Discarded,
    Killed,
    Bewitched,
    Robbed,
    Blackmail { flowered: bool },
    Warrant { signed: bool },
}

impl Marker {
    pub fn is_warrant(&self) -> bool {
        if let Marker::Warrant { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn is_blackmail(&self) -> bool {
        if let Marker::Blackmail { .. } = self {
            true
        } else {
            false
        }
    }
}

pub type PlayerId = String;
pub type Result<T> = std::result::Result<T, &'static str>;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Hash)]
pub struct PlayerName(pub Arc<str>);

impl Borrow<str> for PlayerName {
    fn borrow(&self) -> &str {
        self.0.borrow()
    }
}

impl<T: Into<Arc<str>>> From<T> for PlayerName {
    fn from(str: T) -> Self {
        PlayerName(str.into())
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
