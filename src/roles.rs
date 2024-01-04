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
    Navigator,
    Scholar,
    Warlord,
    Artist,
}

impl fmt::Display for RoleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl RoleName {
    pub fn build_limit(&self) -> usize {
        match self {
            RoleName::Architect => 3,
            RoleName::Navigator => 0,
            RoleName::Scholar => 2,
            _ => 1,
        }
    }
}
