use std::collections::HashMap;

use crate::{
    data,
    lobby::{self, Lobby},
    random,
    types::{Character, District},
};

type PlayerId = String;

pub struct Player {
    pub id: PlayerId,
    pub name: String,
    pub gold: usize,
    pub hand: Vec<District>,
    pub city: Vec<District>,
    pub roles: Vec<Character>,
}

impl Player {
    pub fn new(id: String, name: String) -> Self {
        Player {
            id,
            name,
            gold: 2,
            hand: Vec::new(),
            city: Vec::new(),
            roles: Vec::with_capacity(2),
        }
    }
}

pub struct Game {
    // the top of the deck is the end of the vector
    pub deck: Vec<District>,
    pub players: HashMap<PlayerId, Player>,
    pub seating: Vec<PlayerId>,
    pub characters: Vec<Character>,
    pub crowned: PlayerId,
}

impl Game {
    pub fn new(lobby: Lobby) -> Game {
        let Lobby {
            players,
            mut seating,
        } = lobby;
        random::shuffle(&mut seating);

        let crowned = seating[0].clone();

        let mut unique_districts: Vec<District> = data::districts::UNIQUE.into_iter().collect();
        random::shuffle(&mut unique_districts);

        let mut deck: Vec<District> = data::districts::NORMAL
            .into_iter()
            .flat_map(|(count, district)| std::iter::repeat(district).take(count))
            .chain(unique_districts.into_iter().take(14))
            .collect();
        random::shuffle(&mut deck);

        let players = seating
            .iter()
            .filter_map(|id| players.get(id))
            .map(|p| (p.id.clone(), Player::new(p.id.clone(), p.name.clone())))
            .collect();

        debug_assert!(deck.len() <= 54 + 14);
        Game {
            deck,
            players,
            crowned,
            seating,
            characters: data::characters::CHARACTERS.into_iter().collect(),
        }
    }
}
