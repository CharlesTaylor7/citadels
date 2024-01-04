use crate::{
    actions::{Action, ActionTag},
    data::{self},
    lobby::{self, Lobby},
    random,
    roles::RoleName,
    types::{Character, District, Rank},
};
use macros::tag::Tag;
use rand::prelude::*;
use std::{
    borrow::{Borrow, BorrowMut},
    ops::Deref,
};

type PlayerId = String;
pub type Result<T> = std::result::Result<T, &'static str>;

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

impl Default for Turn {
    fn default() -> Self {
        Turn::Call(0)
    }
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
    pub turn: Vec<Action>,
    pub round: Vec<ActionLog>,
    pub game: Vec<ActionLog>,
}

pub struct ActionLog {
    actor: PlayerId,
    action: Action,
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

    pub fn active_perform_count(&self, action: ActionTag) -> usize {
        self.logs
            .turn
            .iter()
            .filter(|log| log.tag() == action)
            .count()
    }

    pub fn allowed_actions(&self) -> Vec<ActionTag> {
        let mut actions = Vec::new();
        match self.active_turn {
            Turn::Draft(_) => {
                if self.active_perform_count(ActionTag::DraftPick) == 0 {
                    actions.push(ActionTag::DraftPick)
                }
                if self.players.len() == 2
                    && self.active_perform_count(ActionTag::DraftDiscard) == 0
                {
                    actions.push(ActionTag::DraftDiscard)
                }
            }

            Turn::Call(rank) => {
                let mut actions = Vec::new();
                if self.logs.turn.iter().all(|log| {
                    log.tag() != ActionTag::GainCards && log.tag() != ActionTag::GainGold
                }) {
                    actions.push(ActionTag::GainGold);
                    actions.push(ActionTag::GainCards);
                } else {
                    let role = self.characters[rank as usize - 1].borrow();
                    if self.active_perform_count(ActionTag::Build) < role.name.build_limit() {
                        actions.push(ActionTag::Build)
                    }
                    for (n, action) in role.actions {
                        if self.active_perform_count(*action) < *n {
                            actions.push(*action)
                        }
                    }
                }
            }
        }
        actions
    }

    #[must_use]
    pub fn perform(&mut self, action: Action) -> Result<()> {
        self.perform_action(&action)?;

        let end_turn = action.tag() == ActionTag::EndTurn || self.allowed_actions().is_empty();
        self.logs.turn.push(action);

        if end_turn {
            self.end_turn()?;
        }

        Ok(())
    }
    // TODO: convert to result
    #[must_use]
    fn perform_action(&mut self, action: &Action) -> Result<()> {
        match action {
            Action::DraftPick { role } => {
                let player_id = self.active_turn.draft().ok_or("not the draft phase")?;
                let p = self
                    .players
                    .iter_mut()
                    .find(|p| p.id == player_id)
                    .ok_or("player does not exist")?;

                let i = (0..self.draft.remaining.len())
                    .find(|i| self.draft.remaining[*i].name == *role)
                    .ok_or("selected role is not available")?;

                let role = self.draft.remaining.remove(i);
                p.roles.push(role);
            }

            Action::DraftDiscard { role } => {
                let i = (0..self.draft.remaining.len())
                    .find(|i| self.draft.remaining[*i].name == *role)
                    .ok_or("selected role is not available")?;

                self.draft.remaining.remove(i);
            }

            _ => {
                todo!("action is not implemented");
            }
        }
        Ok(())
    }

    #[must_use]
    fn end_turn(&mut self) -> Result<()> {
        // append logs
        for action in std::mem::replace(&mut self.logs.turn, Vec::new()) {
            self.logs.round.push(ActionLog {
                actor: self.active_player().unwrap().name.clone(),
                action,
            });
        }

        match std::mem::take(&mut self.active_turn) {
            Turn::Draft(id) => {
                // advance turn
                let role_count = if self.players.len() <= 3 { 2 } else { 1 };
                self.active_turn = if self.players.iter().all(|p| p.roles.len() == role_count) {
                    Turn::Call(1)
                } else {
                    let index = self
                        .players
                        .iter()
                        .enumerate()
                        .find_map(|(i, p)| if p.id == *id { Some(i) } else { None })
                        .ok_or("impossible: draft player does not exist")?;
                    let next = (index + 1) % self.players.len();
                    Turn::Draft(self.players[next].id.clone())
                };

                // for the 3 player game with 9 characters
                // after the first round of cards are selected,
                // randomly discard 1.
                if self.players.len() == 3
                    && self.characters.len() == 9
                    && self.draft.remaining.len() == 5
                {
                    let mut rng = rand::thread_rng();
                    let index = rng.gen_range(0..self.draft.remaining.len());
                    self.draft.remaining.remove(index);
                }

                // for the 7 player 8 role game, or 8 player 9 role game
                // give the final player the choice to choose the initial discard
                if self.players.len() + 1 == self.characters.len()
                    && self.draft.remaining.len() == 1
                    && self.draft.initial_discard.is_some()
                {
                    let initial = self.draft.initial_discard.take().ok_or("impossible")?;
                    self.draft.remaining.push(initial);
                }
            }
            Turn::Call(rank) => {
                todo!("end call turn")
            }
        }
        Ok(())
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
