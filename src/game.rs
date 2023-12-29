use std::collections::HashMap;

use crate::{
    data,
    lobby::Lobby,
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
        let Lobby { players, seating } = lobby;

        // create players from the lobby, and filter players who were kicked
        let mut players: HashMap<_, _> = seating
            .into_iter()
            .filter_map(|id| players.get(&id))
            .map(|p| (p.id.clone(), Player::new(p.id.clone(), p.name.clone())))
            .collect();

        // create a random seating order based on the map
        let mut seating: Vec<_> = players.keys().cloned().collect();
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
        debug_assert!(deck.len() <= 54 + 14);

        // deal starting hands
        players.values_mut().for_each(|p| {
            let start = deck.len() - 4;
            let end = deck.len();
            println!("{} {} {}", start, end, deck.len());
            for district in deck.drain(start..end) {
                p.hand.push(district);
            }
        });

        Game {
            deck,
            players,
            crowned,
            seating,
            characters: data::characters::CHARACTERS.into_iter().collect(),
        }
    }
}
