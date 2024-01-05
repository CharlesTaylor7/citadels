use std::fmt::{self, Debug};

use crate::{actions::ActionTag, roles::RoleName};

pub type Rank = u8;

#[derive(Clone)]
pub struct Role {
    pub name: RoleName,
    pub rank: Rank,
    pub set: CardSet,
    pub suit: Option<CardSuit>,
    pub description: &'static str,
    pub actions: &'static [(usize, ActionTag)],
}

impl Debug for Role {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

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

#[derive(Debug, Clone, Copy)]
pub enum CardSet {
    Base,
    DarkCity,
    Citadels2016,
    Custom,
}
