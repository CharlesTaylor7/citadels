use crate::districts::{DistrictData, DistrictName};
use crate::{
    actions::{Action, ActionTag},
    data::{self},
    lobby::{self, Lobby},
    random::Prng,
    roles::RoleName,
    types::{CardSet, Rank, Role},
};
use log::*;
use macros::tag::Tag;
use rand::prelude::*;
use rand_core::SeedableRng;
use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
};

type PlayerId = String;
pub type Result<T> = std::result::Result<T, &'static str>;

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct PlayerName(pub String);

impl PlayerName {
    pub fn from(str: String) -> Self {
        PlayerName(str)
    }
}

impl Display for PlayerName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq<PlayerName> for &PlayerName {
    fn eq(&self, other: &PlayerName) -> bool {
        self.0.eq(&other.0)
    }
}

#[derive(Debug)]
pub struct Player {
    pub id: PlayerId,
    pub name: PlayerName,
    pub gold: usize,
    pub hand: Vec<DistrictName>,
    pub city: Vec<CityDistrict>,
    pub roles: Vec<&'static Role>,
}

#[derive(Debug)]
pub struct CityDistrict {
    pub name: DistrictName,
    pub beautified: bool,
}

impl Player {
    pub fn new(id: String, name: PlayerName) -> Self {
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

impl<T> std::fmt::Debug for Deck<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Deck ({})", self.size())
    }
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
    Draft(PlayerName),
    Call(Rank),
}

impl Default for Turn {
    fn default() -> Self {
        Turn::Call(0)
    }
}

impl Turn {
    pub fn draft(&self) -> Option<&PlayerName> {
        if let Turn::Draft(name) = self {
            Some(name)
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

#[derive(Default, Debug)]
pub struct Draft {
    pub remaining: Vec<RoleName>,
    pub initial_discard: Option<RoleName>,
    pub faceup_discard: Vec<RoleName>,
}

#[derive(Default, Debug)]
pub struct Logs {
    pub turn: Vec<ActionLog>,
    pub game: Vec<ActionLog>,
}

pub struct ActionLog {
    player: PlayerName,
    role: Option<RoleName>,
    action: Action,
}

impl ActionLog {
    fn private(&self, f: &mut std::fmt::Formatter<'_>) -> Option<std::fmt::Result> {
        match self.action {
            Action::DraftPick { role } => Some(write!(f, "{} drafted {}", self.player, role)),
            Action::DraftDiscard { role } => Some(write!(f, "{} discarded {}", self.player, role)),

            _ => None,
        }
    }
    fn public(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.action {
            Action::DraftPick { .. } => {
                write!(f, "{} drafted a card.", self.player)
            }

            Action::DraftDiscard { .. } => {
                write!(f, "{} discarded a card.", self.player)
            }

            _ => {
                todo!()
            }
        }
    }
}

impl std::fmt::Display for ActionLog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.public(f)
    }
}

impl std::fmt::Debug for ActionLog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.private(f).unwrap_or_else(|| self.public(f))
    }
}
#[derive(Debug)]
pub struct Game {
    rng: Prng,
    #[cfg(feature = "dev")]
    pub impersonate: Option<PlayerName>,
    pub deck: Deck<DistrictName>,
    pub players: Vec<Player>,
    pub characters: Vec<&'static Role>,
    pub crowned: PlayerName,
    pub active_turn: Turn,
    pub draft: Draft,
    pub logs: Logs,
}

impl Game {
    pub fn active_role(&self) -> Option<&'static Role> {
        let rank = self.active_turn.call()?;
        self.characters.iter().find(|c| c.rank == *rank).copied()
    }

    pub fn start(lobby: Lobby) -> Game {
        let Lobby { mut players } = lobby;

        let mut rng = Prng::from_entropy();

        // randomize the seating order
        players.shuffle(&mut rng);

        // create players from the lobby, and filter players who were kicked
        let mut players: Vec<_> = players
            .into_iter()
            .map(|lobby::Player { id, name }| Player::new(id, name))
            .collect();

        let crowned = players[0].name.clone();

        let mut unique_districts: Vec<DistrictName> = crate::districts::UNIQUE
            .iter()
            .filter_map(|d| {
                if d.set != CardSet::Custom {
                    Some(d.name)
                } else {
                    None
                }
            })
            .collect();
        unique_districts.shuffle(&mut rng);

        let mut deck: Vec<DistrictName> = crate::districts::NORMAL
            .iter()
            .flat_map(|district| {
                let n = district.name.multiplicity();
                std::iter::repeat(district.name).take(n)
            })
            .chain(unique_districts.into_iter().take(14))
            .collect();
        deck.shuffle(&mut rng);

        debug_assert!(
            deck.len() == 68,
            "Deck size is {} but should be 68",
            deck.len()
        );

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
            .iter()
            .take(character_count)
            .collect::<Vec<_>>();

        let mut game = Game {
            #[cfg(feature = "dev")]
            impersonate: None,
            rng,
            players,
            characters,
            draft: Draft::default(),
            deck: Deck::new(deck),
            logs: Logs::default(),
            active_turn: Turn::Draft(crowned.clone()),
            crowned,
        };
        game.begin_draft();
        game
    }

    pub fn begin_draft(&mut self) {
        self.active_turn = Turn::Draft(self.crowned.clone());
        self.draft.remaining = self.characters.iter().map(|c| c.name).collect();

        // discard cards face up in 4+ player game
        if self.players.len() >= 4 {
            for _ in self.players.len() + 2..self.characters.len() {
                let mut index;
                loop {
                    index = self.rng.gen_range(0..self.draft.remaining.len());
                    if self.draft.remaining[index].can_be_discarded_faceup() {
                        break;
                    }
                }

                self.draft
                    .faceup_discard
                    .push(self.draft.remaining.remove(index));
            }
        }

        // discard 1 card facedown
        let i = self.rng.gen_range(0..self.draft.remaining.len());
        self.draft.initial_discard = Some(self.draft.remaining.remove(i));
    }

    pub fn active_player(&self) -> Option<&Player> {
        match &self.active_turn {
            Turn::Draft(name) => self.players.iter().find(move |p| p.name == *name),
            Turn::Call(rank) => self
                .players
                .iter()
                .find(|p| p.roles.iter().any(|role| role.rank == *rank)),
        }
    }

    pub fn active_player_mut(&mut self) -> Option<&mut Player> {
        match &mut self.active_turn {
            Turn::Draft(name) => self.players.iter_mut().find(move |p| p.name == *name),
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
            .filter(|log| log.action.tag() == action)
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
                    log.action.tag() != ActionTag::GainCards
                        && log.action.tag() != ActionTag::GainGold
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
        debug!("Performing {:#?}", action);
        self.perform_action(&action)?;

        let tag = action.tag();
        let player = self.active_player().ok_or("no active player")?;
        let log = ActionLog {
            player: player.name.clone(),
            role: self
                .active_turn
                .call()
                .map(|rank| self.characters[*rank as usize - 1].name),
            action,
        };
        info!("{:#?}", log);

        self.logs.turn.push(log);

        if tag == ActionTag::EndTurn || self.allowed_actions().is_empty() {
            self.end_turn()?;
            self.start_turn()?;
        }

        Ok(())
    }

    fn start_turn(&mut self) -> Result<()> {
        loop {
            if let Some(role) = self.active_role() {
                info!("Calling {}", role.name);
                if self.active_player().is_none() {
                    info!("{} was called; but no one responded", role.name);
                    self.end_turn()?;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    #[must_use]
    fn perform_action(&mut self, action: &Action) -> Result<()> {
        match action {
            Action::DraftPick { role } => {
                let name = self.active_turn.draft().ok_or("not the draft phase")?;
                let p = self
                    .players
                    .iter_mut()
                    .find(|p| p.name == *name)
                    .ok_or("player does not exist")?;

                let i = (0..self.draft.remaining.len())
                    .find(|i| self.draft.remaining[*i] == *role)
                    .ok_or("selected role is not available")?;

                let role = self.draft.remaining.remove(i);
                p.roles.push(role.data());
            }

            Action::DraftDiscard { role } => {
                let i = (0..self.draft.remaining.len())
                    .find(|i| self.draft.remaining[*i] == *role)
                    .ok_or("selected role is not available")?;

                self.draft.remaining.remove(i);
            }

            Action::EndTurn => {
                self.active_turn.call().ok_or("not the call phase")?;

                // this is handled later after the action is appended to the log
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
        self.logs.game.append(&mut self.logs.turn);

        match std::mem::take(&mut self.active_turn) {
            Turn::Draft(name) => {
                // advance turn
                let role_count = if self.players.len() <= 3 { 2 } else { 1 };
                self.active_turn = if self.players.iter().all(|p| p.roles.len() == role_count) {
                    Turn::Call(1)
                } else {
                    let index = self
                        .players
                        .iter()
                        .enumerate()
                        .find_map(|(i, p)| if p.name == name { Some(i) } else { None })
                        .ok_or("impossible: draft player does not exist")?;
                    let next = (index + 1) % self.players.len();
                    Turn::Draft(self.players[next].name.clone())
                };

                // for the 3 player game with 9 characters
                // after the first round of cards are selected,
                // randomly discard 1.
                if self.players.len() == 3
                    && self.characters.len() == 9
                    && self.draft.remaining.len() == 5
                {
                    let index = self.rng.gen_range(0..self.draft.remaining.len());
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
                if rank > self.characters.last().unwrap().rank {
                    self.begin_draft();
                } else {
                    self.active_turn = Turn::Call(rank + 1);
                };
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
