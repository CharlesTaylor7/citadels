use crate::{
    actions::Action,
    data::{self},
    lobby::{self, Lobby},
    random,
    types::{Character, District, Rank},
};
use macros::tag::Tag;
use rand::prelude::*;

type PlayerId = String;

#[derive(Default)]
pub struct Player {
    pub id: PlayerId,
    pub name: String,
    pub gold: usize,
    pub hand: Vec<District>,
    pub city: Vec<District>,
    pub roles: Vec<Character>,
}

// Just the public info
pub struct PlayerInfo<'a> {
    pub id: &'a str,
    pub name: &'a str,
    pub gold: usize,
    pub hand_size: usize,
    pub city: &'a [District],
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

    pub fn info(&self) -> PlayerInfo<'_> {
        let Player {
            id,
            name,
            gold,
            hand,
            city,
            ..
        } = self;
        PlayerInfo {
            id,
            name,
            gold: *gold,
            hand_size: hand.len(),
            city,
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

impl Turn {
    pub fn draft(&self) -> Option<&str> {
        if let Turn::Draft(player_id) = self {
            Some(player_id)
        } else {
            None
        }
    }

    pub fn call(&self) -> Option<&Rank> {
        if let Turn::Call(rank) = self {
            Some(rank)
        } else {
            None
        }
    }
}

#[derive(Default)]
pub struct Draft {
    pub remaining: Vec<Character>,
    pub initial_discard: Option<Character>,
    pub faceup_discard: Vec<Character>,
}

#[derive(Default)]
pub struct Logs {
    pub turn: Vec<ActionLog>,
    pub round: Vec<ActionLog>,
    pub game: Vec<ActionLog>,
}

pub struct ActionLog {
    actor: PlayerId,
    action: Action,
    display: String,
}

pub struct Game {
    pub deck: Deck<District>,
    pub players: Vec<Player>,
    pub characters: Vec<Character>,
    pub crowned: PlayerId,
    pub active_turn: Turn,
    pub draft: Draft,
    pub logs: Logs,
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

        // 9th rank is disallowed for 2
        // 9th rank is required for 3
        // 9th rank is optional for 4-7
        // 9th rank is required for 8
        let character_count = if players.len() == 2 || players.len() == 8 {
            8
        } else {
            9
        };
        let characters = data::characters::CHARACTERS
            .into_iter()
            .take(character_count)
            .collect::<Vec<_>>();

        let mut game = Game {
            players,
            crowned,
            characters,
            draft: Draft::default(),
            deck: Deck::new(deck),
            active_turn: Turn::Draft(String::with_capacity(0)),
            logs: Logs::default(),
        };
        game.begin_draft();
        game
    }

    pub fn begin_draft(&mut self) {
        let mut rng = rand::thread_rng();
        self.active_turn = Turn::Draft(self.crowned.clone());
        self.draft.remaining = self.characters.to_vec();

        // discard cards face up in 4+ player game
        if self.players.len() >= 4 {
            for _ in self.players.len() + 2..self.characters.len() {
                let mut index;
                loop {
                    index = rng.gen_range(0..self.draft.remaining.len());

                    // rank 4 card cannot be discarded faceup
                    if self.draft.remaining[index].rank != 4 {
                        break;
                    }
                }

                self.draft
                    .faceup_discard
                    .push(self.draft.remaining.remove(index));
            }
        }

        // discard 1 card facedown
        let i = rng.gen_range(0..self.draft.remaining.len());
        self.draft.initial_discard = Some(self.draft.remaining.remove(i));
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

    pub fn active_player_mut(&mut self) -> Option<&mut Player> {
        match &mut self.active_turn {
            Turn::Draft(id) => self.players.iter_mut().find(move |p| p.id == *id),
            Turn::Call(rank) => self
                .players
                .iter_mut()
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
