use serde::Deserialize;
use std::fmt;

#[derive(PartialEq, Eq, Copy, Clone, Debug, Deserialize)]
pub enum RoleName {
    Assassin,
    Thief,
    Magician,
    King,
    Bishop,
    Merchant,
    Architect,
    Warlord,
    Artist,
}

impl fmt::Display for RoleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}
