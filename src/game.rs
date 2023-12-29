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

pub struct Game {
    // the top of the deck is the end of the vector
    pub deck: Vec<District>,
    pub players: HashMap<PlayerId, Player>,
    pub seating: Vec<PlayerId>,
    pub characters: Vec<Character>,
    pub crowned: PlayerId,
}

impl Game {
    pub fn new(lobby: &Lobby) -> Option<Game> {
        let mut seating = lobby.seating.clone();
        random::shuffle(&mut seating);

        let crowned = seating.first()?;

        let mut deck: Vec<District> = data::districts::normal_districts
            .into_iter()
            .flat_map(|(count, district)| std::iter::repeat(district).take(count))
            .collect();
        random::shuffle(&mut deck);

        let players = seating
            .iter()
            .filter_map(|id| lobby.players.get(id))
            .map(|p| {
                (
                    p.id.clone(),
                    Player {
                        id: p.id.clone(),
                        name: p.name.clone(),
                        gold: 0,
                        hand: Vec::new(),
                        city: Vec::new(),
                        roles: Vec::new(),
                    },
                )
            })
            .collect();

        Some(Game {
            deck,
            players,
            crowned: crowned.clone(),
            seating,
            characters: data::characters::CHARACTERS.into_iter().collect(),
        })
    }
}
