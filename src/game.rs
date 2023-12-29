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

        let mut unique_districts: Vec<District> =
            data::districts::unique_districts.into_iter().collect();
        random::shuffle(&mut unique_districts);

        let mut deck: Vec<District> = data::districts::normal_districts
            .into_iter()
            .flat_map(|(count, district)| std::iter::repeat(district).take(count))
            .chain(unique_districts.into_iter().take(14))
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

        debug_assert!(
            deck.len() == 54 + std::cmp::max(data::districts::unique_districts.len(), 14)
        );
        Some(Game {
            deck,
            players,
            crowned: crowned.clone(),
            seating,
            characters: data::characters::CHARACTERS.into_iter().collect(),
        })
    }
}
