use crate::{
    data::{self, characters},
    lobby::{self, Lobby},
    random,
    types::{Character, District, Rank},
};
use rand::{distributions::Uniform, prelude::*};

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

pub struct Deck<T> {
    deck: Vec<T>,
    discard: Vec<T>,
}
impl<T> Deck<T> {
    pub fn size(&self) -> usize {
        self.deck.len() + self.discard.len()
    }
    pub fn new(deck: Vec<T>) -> Self {
        Self {
            deck,
            discard: Vec::new(),
        }
    }

    pub fn draw(&mut self) -> Option<T> {
        if let Some(card) = self.deck.pop() {
            Some(card)
        } else {
            std::mem::swap(&mut self.deck, &mut self.discard);
            self.deck.reverse();
            self.deck.pop()
        }
    }

    pub fn discard_to_bottom(&mut self, card: T) {
        self.discard.push(card);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Turn {
    Draft(PlayerId),
    Call(Rank),
}

pub struct Game {
    pub deck: Deck<District>,
    pub players: Vec<Player>,
    pub characters: Vec<Character>,
    pub crowned: PlayerId,
    pub active_turn: Turn,
    pub to_draft: Vec<Character>,
    pub discarded_facedown: Vec<Character>,
    pub discarded_faceup: Vec<Character>,
}

impl Game {
    pub fn start(lobby: Lobby) -> Game {
        let Lobby { mut players } = lobby;

        // randomize the seating order
        random::shuffle(&mut players);

        // create players from the lobby, and filter players who were kicked
        let mut players: Vec<_> = players
            .into_iter()
            .map(|lobby::Player { id, name }| Player::new(id, name))
            .collect();

        let crowned = players[0].id.clone();

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
        players.iter_mut().for_each(|p| {
            let start = deck.len() - 4;
            let end = deck.len();
            for district in deck.drain(start..end) {
                p.hand.push(district);
            }
        });

        let mut game = Game {
            players,
            crowned,
            to_draft: Vec::with_capacity(9),
            discarded_faceup: Vec::with_capacity(3),
            discarded_facedown: Vec::with_capacity(3),
            deck: Deck::new(deck),
            active_turn: Turn::Draft(String::with_capacity(0)),
            characters: data::characters::CHARACTERS.into_iter().collect(),
        };
        game.begin_draft();
        game
    }

    pub fn begin_draft(&mut self) {
        let mut rng = rand::thread_rng();
        self.active_turn = Turn::Draft(self.crowned.clone());
        self.to_draft = self.characters.clone();

        // discard cards face up in 4+ player game
        if self.players.len() >= 4 {
            for _ in self.players.len() + 2..self.characters.len() {
                let i = rng.gen_range(0..self.to_draft.len());
                self.discarded_faceup.push(self.to_draft.remove(i));
            }
        }

        // discard 1 card facedown
        let i = rng.gen_range(0..self.to_draft.len());
        self.discarded_facedown.push(self.to_draft.remove(i));
    }

    pub fn active_player(&self) -> Option<&Player> {
        match &self.active_turn {
            Turn::Draft(id) => self.players.iter().find(move |p| p.id == *id),
            Turn::Call(rank) => self
                .players
                .iter()
                .find(|p| p.roles.iter().any(|role| role.rank == *rank)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::game::*;

    #[test]
    fn test_deck() {
        let mut deck: Deck<usize> = Deck::new(vec![3, 2, 1]);
        assert_eq!(deck.draw(), Some(1));
        deck.discard_to_bottom(4);
        deck.discard_to_bottom(5);
        assert_eq!(deck.draw(), Some(2));
        assert_eq!(deck.draw(), Some(3));
        assert_eq!(deck.draw(), Some(4));
        assert_eq!(deck.draw(), Some(5));
        assert_eq!(deck.draw(), None);
    }
}
