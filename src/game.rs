use std::collections::HashMap;

use crate::types::{District, Character};

type PlayerId = String;

pub struct Player {
    id: PlayerId,
    name: String,
    gold: usize,
    hand: Vec<District>,
    city: Vec<District>,
    roles: Vec<Character>,
}

#[derive(Default)]
pub struct Game {
    players: HashMap<PlayerId, Player>,
    seating: Vec<PlayerId>,
    characters: Vec<Character>,
    crowned: PlayerId,
}
