use std::collections::*;

type PlayerId = String;

#[derive(serde::Serialize, Clone)]
pub struct Player {
    pub id: PlayerId,
    pub name: String,
}

pub struct Lobby {
    pub players: HashMap<PlayerId, Player>,
    pub seating: Vec<PlayerId>,
}

impl Default for Lobby {
    fn default() -> Self {
        Self {
            players: HashMap::new(),
            seating: Vec::new(),
        }
    }
}
