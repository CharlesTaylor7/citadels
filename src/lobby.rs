use std::collections::*;

pub struct PlayerId(String);

pub struct Player {
    id: PlayerId,
    name: String,
}

pub struct Lobby {
    players: HashMap<PlayerId, Player>,
    seating: Vec<PlayerId>,
}

impl Default for Lobby {
    fn default() -> Self {
        Self {
            players: HashMap::new(),
            seating: Vec::new(),
        }
    }
}
