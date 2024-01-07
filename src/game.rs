use crate::actions::{Resource, Select};
use crate::districts::DistrictName;
use crate::roles::Rank;
use crate::types::{CardSuit, Marker};
use crate::{
    actions::{Action, ActionTag},
    lobby::{self, Lobby},
    random::Prng,
    roles::RoleName,
    types::CardSet,
};
use log::*;
use macros::tag::Tag;
use rand::prelude::*;
use rand_core::SeedableRng;
use serde::Deserialize;
use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
};

type PlayerId = String;
pub type Result<T> = std::result::Result<T, &'static str>;

#[derive(Default, Debug, PartialEq, Eq, Clone, Deserialize)]
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
    pub roles: Vec<RoleName>,
}

#[derive(Debug)]
pub struct CityDistrict {
    pub name: DistrictName,
    pub beautified: bool,
}

impl CityDistrict {
    pub fn from(name: DistrictName) -> Self {
        Self {
            name,
            beautified: false,
        }
    }
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Step {
    GainResource,
    Main,
    EndOfTurn,
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

    pub fn call(&self) -> Option<Rank> {
        if let Turn::Call(rank) = self {
            Some(*rank)
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

impl Draft {
    pub fn clear(&mut self) {
        self.remaining.clear();
        self.initial_discard = None;
        self.faceup_discard.clear();
    }
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

            _ => {
                debug!("Warning: default case for {:#?}", self.action);
                None
            }
        }
    }

    fn public(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.action {
            Action::DraftPick { .. } => {
                write!(f, "{} drafted a card.", self.player)
            }

            Action::DraftDiscard { .. } => {
                write!(f, "{} discarded a card.", self.player)
            }

            Action::GatherResources {
                resource: Resource::Cards,
            } => {
                write!(f, "Resource step: {} is picking card(s).", self.player)
            }

            Action::GatherCardsPick { district } => {
                write!(
                    f,
                    "Resource step: {} picked {} card(s).",
                    self.player,
                    district.len()
                )
            }

            Action::GatherResources {
                resource: Resource::Gold,
            } => {
                write!(f, "Resource step: {} gained gold.", self.player)
            }

            Action::EndTurn { .. } => {
                write!(f, "{} ended their turn.", self.player)
            }

            _ => {
                debug!("Warning: default case for {:#?}", self.action);
                write!(f, "{:#?}", self.action)
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
pub struct FollowupAction {
    pub action: ActionTag,
    pub context: FollowupContext,
}

#[derive(Debug, Tag)]
pub enum FollowupContext {
    PickDistrict(Vec<DistrictName>),
}

#[derive(Debug)]
pub struct GameRole {
    pub role: RoleName,
    pub markers: Vec<Marker>,
}

#[derive(Debug)]
pub struct Game {
    rng: Prng,
    pub deck: Deck<DistrictName>,
    pub players: Vec<Player>,
    pub characters: Vec<GameRole>,
    pub crowned: PlayerName,
    pub active_turn: Turn,
    pub draft: Draft,
    pub logs: Logs,
    pub followup: Option<FollowupAction>,
}

impl Game {
    #[cfg(feature = "dev")]
    pub fn default_game() -> Option<Game> {
        let mut game = Game::start(Lobby::demo(vec!["Alph", "Brittany", "Charlie"]));
        for p in game.players.iter_mut() {
            p.hand.push(DistrictName::SecretVault);
        }

        // deal roles out randomly
        let mut cs: Vec<_> = game.characters.iter().map(|c| c.role).collect();
        cs.shuffle(&mut game.rng);

        for p in game.players.iter_mut() {
            p.roles.push(cs.pop().unwrap().clone());
            p.roles.push(cs.pop().unwrap().clone());
            p.roles.sort_by_key(|c| c.rank());
        }
        game.active_turn = Turn::Call(1);
        game.start_turn().ok();

        Some(game)
    }

    #[cfg(not(feature = "dev"))]
    pub fn default_game() -> Option<Game> {
        None
    }

    pub fn active_role(&self) -> Option<&GameRole> {
        let rank = self.active_turn.call()?;
        self.characters.iter().find(|c| c.role.rank() == rank)
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
        let characters = crate::roles::select(&mut rng, players.len())
            .map(|role| GameRole {
                role,
                markers: Vec::with_capacity(2),
            })
            .collect();
        let mut game = Game {
            rng,
            players,
            draft: Draft::default(),
            deck: Deck::new(deck),
            logs: Logs::default(),
            active_turn: Turn::Draft(crowned.clone()),
            crowned,
            characters,
            followup: None,
        };
        game.begin_draft();
        game
    }

    pub fn begin_draft(&mut self) {
        self.active_turn = Turn::Draft(self.crowned.clone());
        self.draft.remaining = self.characters.iter().map(|c| c.role).collect();

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
                .find(|p| p.roles.iter().any(|role| role.rank() == *rank)),
        }
    }

    pub fn active_player_mut(&mut self) -> Option<&mut Player> {
        match &mut self.active_turn {
            Turn::Draft(name) => self.players.iter_mut().find(move |p| p.name == *name),
            Turn::Call(rank) => self
                .players
                .iter_mut()
                .find(|p| p.roles.iter().any(|role| role.rank() == *rank)),
        }
    }

    pub fn active_perform_count(&self, action: ActionTag) -> usize {
        self.logs
            .turn
            .iter()
            .filter(|log| log.action.tag() == action)
            .count()
    }

    fn has_done<F: Fn(&Action) -> bool>(&self, f: F) -> bool {
        self.logs.turn.iter().any(|log| f(&log.action))
    }

    fn has_not_done<F: Fn(&Action) -> bool>(&self, f: F) -> bool {
        self.logs.turn.iter().all(|log| !f(&log.action))
    }

    pub fn allowed_actions(&self) -> Vec<ActionTag> {
        if let Some(f) = &self.followup {
            return vec![f.action];
        }

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
                let c = self.characters[rank as usize - 1].borrow();

                for (n, action) in c.role.data().actions {
                    if self.active_perform_count(*action) < *n {
                        actions.push(*action)
                    }
                }

                // You have to gather resources before building
                if self.has_not_done(|action| action.tag() == ActionTag::GatherResources) {
                    // gather
                    actions.push(ActionTag::GatherResources);
                } else if self.active_perform_count(ActionTag::Build) < c.role.build_limit() {
                    // build
                    actions.push(ActionTag::Build)
                }

                if actions.iter().all(|action| !action.is_required()) {
                    actions.push(ActionTag::EndTurn);
                }
            }
        }
        info!("Available actions: {:#?}", actions);
        actions
    }

    #[must_use]
    pub fn perform(&mut self, action: Action) -> Result<()> {
        self.active_player().ok_or("no active player")?;
        let allowed = self.allowed_actions();
        if !allowed.contains(&action.tag()) {
            return Err("Action is not allowed");
        }

        self.followup = self.perform_action(&action)?;

        let player = self.active_player().ok_or("no active player")?;
        let tag = action.tag();
        let log = ActionLog {
            player: player.name.clone(),
            role: self.active_role().map(|c| c.role),
            action,
        };
        info!("{:#?}", log);
        if self.followup.is_some() {
            info!("followup: {:#?}", self.followup);
        }

        self.logs.turn.push(log);

        if tag == ActionTag::EndTurn
            || self
                .allowed_actions()
                .iter()
                .all(|action| *action == ActionTag::EndTurn)
        {
            self.end_turn()?;
            self.start_turn()?;
        }

        Ok(())
    }

    fn start_turn(&mut self) -> Result<()> {
        loop {
            if let Some(c) = self.active_role() {
                info!("Calling {}", c.role);
                if self.active_player().is_none() {
                    info!("{} was called; but no one responded", c.role);
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
    fn perform_action(&mut self, action: &Action) -> Result<Option<FollowupAction>> {
        let followup = match action {
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
                p.roles.push(role);
                None
            }

            Action::DraftDiscard { role } => {
                let i = (0..self.draft.remaining.len())
                    .find(|i| self.draft.remaining[*i] == *role)
                    .ok_or("selected role is not available")?;

                self.draft.remaining.remove(i);
                None
            }

            Action::EndTurn => {
                self.active_turn.call().ok_or("not the call phase")?;

                None
                // this is handled later after the action is appended to the log
            }

            Action::GatherResources {
                resource: Resource::Gold,
            } => {
                let player = self.active_player_mut().ok_or("no active player")?;
                player.gold += 2; // roles / districts may affect this
                None
            }

            Action::GatherResources {
                resource: Resource::Cards,
            } => {
                let draw_amount = 2; // roles / disricts may affect this
                let mut cards = Vec::with_capacity(draw_amount);
                for _ in 0..draw_amount {
                    if let Some(card) = self.deck.draw() {
                        cards.push(card);
                    } else {
                        break;
                    }
                }

                if cards.len() > 0 {
                    Some(FollowupAction {
                        action: ActionTag::GatherCardsPick,
                        context: FollowupContext::PickDistrict(cards),
                    })
                } else {
                    warn!("No cards left in the deck");
                    None
                }
            }

            Action::GatherCardsPick { district } => {
                let followup = self.followup.take().ok_or("action is not allowed")?;
                let FollowupContext::PickDistrict(mut options) = followup.context;
                if let Select::Single(district) = district {
                    Game::remove_first(&mut options, *district).ok_or("invalid choice")?;
                    let player = self.active_player_mut().ok_or("no active player")?;
                    player.hand.push(*district);

                    options.shuffle(&mut self.rng);
                    for remaining in options {
                        self.deck.discard_to_bottom(remaining);
                    }
                    return Ok(None);
                } else {
                    return Err("cannot pick more than 1");
                }
            }
            Action::GoldFromNobility => self.gain_gold_for_suit(CardSuit::Noble)?,
            Action::GoldFromReligion => self.gain_gold_for_suit(CardSuit::Religious)?,
            Action::GoldFromTrade => self.gain_gold_for_suit(CardSuit::Trade)?,
            Action::GoldFromMilitary => self.gain_gold_for_suit(CardSuit::Military)?,

            Action::MerchantGainOneGold => {
                self.active_player_mut().unwrap().gold += 1;
                None
            }
            Action::ArchitectGainCards => {
                for _ in 0..2 {
                    if let Some(district) = self.deck.draw() {
                        self.active_player_mut().unwrap().hand.push(district);
                    }
                }
                None
            }
            Action::Build { district } => {
                let player = self.active_player_mut().ok_or("no active player")?;
                let cost = district.data().cost;
                if cost > player.gold {
                    return Err("not enough gold");
                }
                if player.city.iter().any(|d| d.name == *district) {
                    return Err("cannot build duplicate");
                }
                Game::remove_first(&mut player.hand, *district).ok_or("card not in hand")?;
                player.gold -= cost;
                player.city.push(CityDistrict::from(*district));
                None
            }
            _ => {
                // todo
                return Err("action is not implemented");
            }
        };
        Ok(followup)
    }

    fn remove_first<T: PartialEq>(items: &mut Vec<T>, item: T) -> Option<T> {
        let index = items
            .iter()
            .enumerate()
            .find_map(|(i, v)| if item == *v { Some(i) } else { None })?;
        Some(items.remove(index))
    }

    fn gain_gold_for_suit(&mut self, suit: CardSuit) -> Result<Option<FollowupAction>> {
        let player = self.active_player_mut().ok_or("no active player")?;
        player.gold += player
            .city
            .iter()
            .filter(|c| c.name.data().suit == suit)
            .count();

        Ok(None)
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
                if self.characters.last().is_some_and(|c| rank < c.role.rank()) {
                    self.active_turn = Turn::Call(rank + 1);
                } else {
                    self.end_round();
                };
            }
        }
        Ok(())
    }

    pub fn end_round(&mut self) {
        self.draft.clear();

        for character in self.characters.iter_mut() {
            character.markers.clear();
        }
        self.begin_draft();
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
