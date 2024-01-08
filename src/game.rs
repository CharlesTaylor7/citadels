use crate::actions::{Action, ActionTag, MagicianAction, Resource, Select};
use crate::districts::DistrictName;
use crate::lobby::{self, Lobby};
use crate::random::Prng;
use crate::roles::{Rank, RoleName};
use crate::types::{CardSet, CardSuit, Marker, PlayerId, PlayerName};
use log::*;
use macros::tag::Tag;
use rand::prelude::*;
use rand_core::SeedableRng;

use std::{borrow::Borrow, fmt::Debug};

pub type Result<T> = std::result::Result<T, &'static str>;

#[derive(Debug)]
pub struct Player {
    pub id: PlayerId,
    pub name: PlayerName,
    pub gold: usize,
    pub hand: Vec<DistrictName>,
    pub city: Vec<CityDistrict>,
    pub roles: Vec<RoleName>,
}

impl Player {
    pub fn city_has(&self, name: DistrictName) -> bool {
        self.city.iter().any(|c| c.name == name)
    }
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

    pub fn draw_many(&mut self, amount: usize) -> impl Iterator<Item = T> + '_ {
        (0..amount).flat_map(|_| self.draw())
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

#[derive(Debug)]
#[allow(dead_code)]
pub struct Log {
    source: Source,
    display: String,
}

#[derive(Debug)]
pub enum Source {
    Triggered,
    Action(PlayerName, Action),
}

#[derive(Debug)]
pub struct FollowupAction {
    pub action: ActionTag,
    pub revealed: Vec<DistrictName>,
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
    pub followup: Option<FollowupAction>,
    pub turn_actions: Vec<Action>,
    pub logs: Vec<Log>,
}

pub type ActionResult = Result<ActionOutput>;

pub struct ActionOutput {
    pub followup: Option<FollowupAction>,
    pub log: String,
}

impl Game {
    pub fn complete_city_size(&self) -> usize {
        if self.players.len() <= 3 {
            8
        } else {
            7
        }
    }

    #[cfg(feature = "dev")]
    pub fn default_game() -> Option<Game> {
        let mut game = Game::start(Lobby::demo(vec!["Alph", "Brittany", "Charlie"]));
        // deal roles out randomly
        let mut cs: Vec<_> = game.characters.iter().map(|c| c.role).collect();
        cs.shuffle(&mut game.rng);

        for p in game.players.iter_mut() {
            p.roles.push(cs.pop().unwrap());
            p.roles.push(cs.pop().unwrap());
            p.roles.sort_by_key(|c| c.rank());
        }
        game.active_turn = Turn::Call(1);
        game.start_turn().ok()?;

        Some(game)
    }

    #[cfg(not(feature = "dev"))]
    pub fn default_game() -> Option<Game> {
        None
    }

    pub fn active_role(&self) -> Result<&GameRole> {
        let rank = self.active_turn.call().ok_or("not the call phase")?;
        self.characters
            .iter()
            .find(|c| c.role.rank() == rank)
            .ok_or("no active role")
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
            .filter_map(|d| if d.name.enabled() { Some(d.name) } else { None })
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
            active_turn: Turn::Draft(crowned.clone()),
            crowned,
            characters,
            followup: None,
            turn_actions: Vec::new(),
            logs: Vec::with_capacity(1000),
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

    pub fn active_player(&self) -> Result<&Player> {
        let option = match &self.active_turn {
            Turn::Draft(name) => self.players.iter().find(move |p| p.name == *name),
            Turn::Call(rank) => self
                .players
                .iter()
                .find(|p| p.roles.iter().any(|role| role.rank() == *rank)),
        };
        option.ok_or("no active player")
    }

    pub fn active_player_mut(&mut self) -> Result<&mut Player> {
        let option = match &mut self.active_turn {
            Turn::Draft(name) => self.players.iter_mut().find(move |p| p.name == *name),
            Turn::Call(rank) => self
                .players
                .iter_mut()
                .find(|p| p.roles.iter().any(|role| role.rank() == *rank)),
        };
        option.ok_or("no active player")
    }

    pub fn active_perform_count(&self, action: ActionTag) -> usize {
        self.turn_actions
            .iter()
            .filter(|act| act.tag() == action)
            .count()
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
                if self.active_perform_count(ActionTag::GatherResources) < 1 {
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

    pub fn perform(&mut self, action: Action) -> Result<()> {
        self.active_player()?;
        let allowed = self.allowed_actions();
        if !allowed.contains(&action.tag()) {
            return Err("Action is not allowed");
        }

        let ActionOutput { followup, log } = self.perform_action(&action)?;
        self.followup = followup;

        let player = self.active_player()?;
        let tag = action.tag();
        let log = Log {
            source: Source::Action(player.name.clone(), action),
            display: log,
        };
        info!("{:#?}", log);
        if self.followup.is_some() {
            info!("followup: {:#?}", self.followup);
        }
        self.logs.push(log);

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
        let mut logs = Vec::new();
        while let Ok(c) = self.active_role() {
            let role = c.role.clone();
            logs.push(format!("Calling {}", c.role.display_name()));

            if c.markers.iter().any(|m| *m == Marker::Assassinated) {
                logs.push(format!("They were killed; their turn is skipped."));
                continue;
            }

            if let Ok(player) = self.active_player() {
                logs.push(format!("{} started their turn.", player.name));

                if c.markers.iter().any(|m| *m == Marker::Robbed) {
                    let gold = player.gold;
                    self.active_player_mut().unwrap().gold = 0;
                    let thief = self
                        .players
                        .iter_mut()
                        .find(|p| p.roles.iter().any(|r| *r == role))
                        .unwrap();
                    thief.gold += gold;
                    let thief_name = thief.name.clone();

                    logs.push(format!(
                        "{} ({}) was robbed; they forfeit all their gold to {}.",
                        role.display_name(),
                        self.active_player().unwrap().name,
                        thief_name
                    ));
                }

                break;
            }

            logs.push(format!("no one responded"));

            self.end_turn()?;
        }

        for item in logs {
            info!("{}", item);
            self.logs.push(Log {
                source: Source::Triggered,
                display: item,
            });
        }

        Ok(())
    }

    fn perform_action(&mut self, action: &Action) -> ActionResult {
        Ok(match action {
            Action::DraftPick { role } => {
                Game::remove_first(&mut self.draft.remaining, *role);
                let player = self.active_player_mut()?;
                player.roles.push(*role);

                ActionOutput {
                    log: format!("{} drafted a role.", player.name),
                    followup: None,
                }
            }

            Action::DraftDiscard { role } => {
                let i = (0..self.draft.remaining.len())
                    .find(|i| self.draft.remaining[*i] == *role)
                    .ok_or("selected role is not available")?;

                self.draft.remaining.remove(i);
                ActionOutput {
                    log: format!("{} discarded a role face down.", self.active_player()?.name),
                    followup: None,
                }
            }

            Action::EndTurn => {
                // this is handled after the logs are updated.
                ActionOutput {
                    log: format!("{} ended their turn.", self.active_player()?.name),
                    followup: None,
                }
            }

            Action::GatherResources {
                resource: Resource::Gold,
            } => {
                let player = self.active_player_mut()?;
                player.gold += 2;
                ActionOutput {
                    log: format!("{} gained 2 gold.", player.name),
                    followup: None,
                }
            }

            Action::GatherResources {
                resource: Resource::Cards,
            } => {
                let mut draw_amount = 2; // roles / disricts may affect this
                if self.active_player()?.city_has(DistrictName::Observatory) {
                    draw_amount += 1;
                }

                let mut drawn = self.deck.draw_many(draw_amount).collect();

                if self.active_player()?.city_has(DistrictName::Library) {
                    self.active_player_mut()?.hand.append(&mut drawn);

                    ActionOutput {
                        log: format!(
                            "{} is gathering cards. With their library they kept all {} cards.",
                            self.active_player()?.name,
                            draw_amount
                        ),
                        followup: None,
                    }
                } else {
                    ActionOutput {
                        log: format!(
                        "{} is gathering cards. They revealed {} cards from the top of the deck.",
                        self.active_player()?.name,
                        draw_amount
                    ),
                        followup: if drawn.len() > 0 {
                            Some(FollowupAction {
                                action: ActionTag::GatherCardsPick,
                                revealed: drawn,
                            })
                        } else {
                            None
                        },
                    }
                }
            }

            Action::GatherCardsPick { district } => {
                let FollowupAction { mut revealed, .. } =
                    self.followup.take().ok_or("action is not allowed")?;

                if let Select::Single(district) = district {
                    Game::remove_first(&mut revealed, *district).ok_or("invalid choice")?;
                    self.active_player_mut()?.hand.push(*district);

                    revealed.shuffle(&mut self.rng);
                    for remaining in revealed {
                        self.deck.discard_to_bottom(remaining);
                    }
                    let player = self.active_player()?;
                    ActionOutput {
                        log: format!("{} picked {} card(s).", player.name, 1),
                        followup: None,
                    }
                } else {
                    return Err("cannot pick more than 1");
                }
            }
            Action::GoldFromNobility => self.gain_gold_for_suit(CardSuit::Noble)?,
            Action::GoldFromReligion => self.gain_gold_for_suit(CardSuit::Religious)?,
            Action::GoldFromTrade => self.gain_gold_for_suit(CardSuit::Trade)?,
            Action::GoldFromMilitary => self.gain_gold_for_suit(CardSuit::Military)?,

            Action::CardsFromNobility => self.gain_cards_for_suit(CardSuit::Noble)?,
            Action::CardsFromReligious => self.gain_cards_for_suit(CardSuit::Religious)?,

            Action::MerchantGainOneGold => {
                let player = self.active_player_mut()?;
                player.gold += 1;
                ActionOutput {
                    log: format!("The Merchant ({}) gained 1 extra gold.", player.name),
                    followup: None,
                }
            }
            Action::ArchitectGainCards => {
                self.gain_cards(2);
                let player = self.active_player()?;
                ActionOutput {
                    log: format!("The Architect ({}) gained 2 extra cards.", player.name),
                    followup: None,
                }
            }
            Action::Build { district } => {
                let player = self.active_player_mut()?;
                let district = district.data();

                let mut cost = district.cost;
                if district.suit == CardSuit::Unique
                    && player.city.iter().any(|d| d.name == DistrictName::Factory)
                {
                    cost -= 1;
                }

                if cost > player.gold {
                    return Err("not enough gold");
                }

                if !player.city_has(DistrictName::Quarry)
                    && player.city.iter().any(|d| d.name == district.name)
                {
                    return Err("cannot build duplicate");
                }

                Game::remove_first(&mut player.hand, district.name).ok_or("card not in hand")?;
                player.gold -= cost;
                player.city.push(CityDistrict::from(district.name));

                ActionOutput {
                    log: format!("{} built a {}.", player.name, district.display_name),
                    followup: None,
                }
            }

            Action::TakeCrown => {
                self.crowned = self.active_player()?.name.clone();

                let active = self.active_role()?;
                let player = self.active_player()?;
                ActionOutput {
                    log: format!(
                        "The {} ({}) took the crown.",
                        active.role.display_name(),
                        player.name,
                    ),
                    followup: None,
                }
            }

            Action::Assassinate { role } => {
                let target = self
                    .characters
                    .iter_mut()
                    .find(|c| c.role == *role)
                    .ok_or("target is not valid")?;

                target.markers.push(Marker::Assassinated);

                ActionOutput {
                    log: format!(
                        "The Assassin ({}) killed the {}; Their turn will be skipped.",
                        self.active_player()?.name,
                        role.display_name(),
                    ),
                    followup: None,
                }
            }
            Action::Steal { role } => {
                if role.rank() < 3 {
                    return Err("target rank is too low");
                }

                let target = self
                    .characters
                    .iter_mut()
                    .find(|c| c.role == *role)
                    .ok_or("target is not valid")?;

                if target
                    .markers
                    .iter()
                    .any(|marker| *marker == Marker::Assassinated)
                {
                    return Err("cannot rob from the dead");
                }
                target.markers.push(Marker::Robbed);

                ActionOutput {
                    log: format!(
                        "The Thief ({}) robbed the {}; Their gold will be taken at the start of their turn.",
                        self.active_player()?.name,
                        role.display_name(),
                    ),
                    followup: None,
                }
            }

            Action::Magic(MagicianAction::TargetPlayer { player }) => {
                let mut hand = std::mem::take(&mut self.active_player_mut()?.hand);
                let target = if let Some(p) = self.players.iter_mut().find(|p| p.name == *player) {
                    p
                } else {
                    // put the hand back;
                    self.active_player_mut()?.hand = hand;
                    return Err("invalid target");
                };

                let hand_count = hand.len();
                let target_count = target.hand.len();

                std::mem::swap(&mut hand, &mut target.hand);
                self.active_player_mut()?.hand = hand;

                ActionOutput {
                    log: format!(
                        "The Magician ({}) swapped their hand of {} cards with {}'s hand of {} cards.",
                        self.active_player()?.name,
                        hand_count,
                        player,
                        target_count,

                    ),
                    followup: None,
                }
            }

            Action::Magic(MagicianAction::TargetDeck { discard }) => {
                let active = self.active_player_mut()?;
                let discard = discard.to_vec();
                let backup = active.hand.clone();

                for card in discard.iter() {
                    if Game::remove_first(&mut active.hand, *card).is_none() {
                        active.hand = backup;
                        return Err("Can't discard a card that's not in your hand");
                    }
                }

                for card in discard.iter() {
                    self.deck.discard_to_bottom(*card);
                }

                self.gain_cards(discard.len());

                ActionOutput {
                    log: format!(
                        "The Magician ({}) discarded {} cards and drew {} more.",
                        self.active_player()?.name,
                        discard.len(),
                        discard.len(),
                    ),
                    followup: None,
                }
            }

            Action::Destroy(target) => {
                if target.district == DistrictName::Keep {
                    return Err("cannot destroy the Keep");
                }

                let available_gold = self.active_player()?.gold;
                let complete_size = self.complete_city_size();
                let targeted_player = self
                    .players
                    .iter_mut()
                    .find(|p| p.city.len() < complete_size && p.name == target.player)
                    .ok_or("invalid player target")?;

                if targeted_player.roles.iter().any(|r| *r == RoleName::Bishop) {
                    return Err("cannot target the Bishop's city");
                }

                if target.district == DistrictName::Keep {
                    return Err("cannot target the Keep");
                }

                let city_index = targeted_player
                    .city
                    .iter()
                    .enumerate()
                    .find_map(|(i, c)| {
                        if c.name == target.district && c.beautified == target.beautified {
                            Some(i)
                        } else {
                            None
                        }
                    })
                    .ok_or("does not exist in the targeted player's city")?;

                let destroy_cost = target.effective_cost(targeted_player) - 1;
                if available_gold < destroy_cost {
                    return Err("not enough gold to destroy");
                }

                targeted_player.city.remove(city_index);
                self.active_player_mut()?.gold -= destroy_cost;
                self.deck.discard_to_bottom(target.district);

                ActionOutput {
                    log: format!(
                        "The Warlord ({}) destroyed {}'s {}.",
                        self.active_player()?.name,
                        target.player,
                        target.district.data().display_name,
                    ),
                    followup: None,
                }
            }

            Action::Beautify { district } => {
                let player = self.active_player_mut()?;

                if player.gold < 1 {
                    return Err("not enough gold");
                }

                let city_district = player
                    .city
                    .iter_mut()
                    .find(|d| !d.beautified && d.name == *district)
                    .ok_or("invalid target; is it already beautified?")?;

                city_district.beautified = true;
                player.gold -= 1;

                ActionOutput {
                    log: format!(
                        "The Artist ({}) beautified their {}.",
                        self.active_player()?.name,
                        district.data().display_name,
                    ),
                    followup: None,
                }
            }

            Action::NavigatorGain {
                resource: Resource::Cards,
            } => {
                self.gain_cards(4);
                ActionOutput {
                    log: format!(
                        "The Navigator ({}) gained 4 extra cards.",
                        self.active_player()?.name
                    ),
                    followup: None,
                }
            }

            Action::NavigatorGain {
                resource: Resource::Gold,
            } => {
                let player = self.active_player_mut()?;
                player.gold += 4;

                ActionOutput {
                    log: format!("The Navigator ({}) gained 4 extra gold.", player.name),
                    followup: None,
                }
            }

            Action::SeerTake { .. } => {
                todo!()
            }

            Action::SeerDistribute { .. } => {
                todo!()
            }

            Action::WizardPeek { .. } => {
                todo!()
            }
            Action::WizardPick { .. } => {
                todo!()
            }

            Action::ResourcesFromReligious { .. } => {
                todo!()
            }
            Action::EmperorAssignCrown { .. } => {
                todo!()
            }
            Action::QueenGainGold { .. } => {
                todo!()
            }
            Action::Spy { .. } => {
                todo!()
            }
            Action::CollectTaxes { .. } => {
                todo!()
            }
            Action::Bewitch { .. } => {
                todo!()
            }
            Action::Seize { .. } => {
                todo!()
            }

            Action::TakeFromRich { .. } => {
                todo!()
            }
            Action::SendWarrants { .. } => {
                todo!()
            }
            Action::Threaten { .. } => {
                todo!()
            }
            Action::ExchangeCityDistricts { .. } => {
                todo!()
            }

            Action::ScholarReveal { .. } => {
                todo!()
            }
            Action::ScholarPick { .. } => {
                todo!()
            }
        })
    }

    fn remove_first<T: PartialEq>(items: &mut Vec<T>, item: T) -> Option<T> {
        let index = items
            .iter()
            .enumerate()
            .find_map(|(i, v)| if item == *v { Some(i) } else { None })?;
        Some(items.remove(index))
    }

    fn gain_cards(&mut self, amount: usize) -> usize {
        let mut tally = 0;
        for _ in 0..amount {
            if let Some(district) = self.deck.draw() {
                let player = self.active_player_mut().unwrap();
                player.hand.push(district);
                tally += 1;
            } else {
                break;
            }
        }
        tally
    }

    fn gain_gold_for_suit(&mut self, suit: CardSuit) -> ActionResult {
        let player = self.active_player_mut()?;
        let amount = player
            .city
            .iter()
            .filter(|c| c.name.data().suit == suit || c.name == DistrictName::SchoolOfMagic)
            .count();

        player.gold += amount;

        Ok(ActionOutput {
            followup: None,
            log: format!(
                "{} gained {} gold from their {} districts",
                player.name, amount, suit
            ),
        })
    }

    fn gain_cards_for_suit(&mut self, suit: CardSuit) -> Result<ActionOutput> {
        let player = self.active_player()?;
        let count = player
            .city
            .iter()
            .filter(|c| c.name.data().suit == suit || c.name == DistrictName::SchoolOfMagic)
            .count();

        // they may have drawn less cards then the number of districts
        // if the deck was low on cards.
        let amount = self.gain_cards(count);
        let player = self.active_player()?;

        Ok(ActionOutput {
            followup: None,
            log: format!(
                "{} gained {} cards from their {} districts",
                player.name, amount, suit
            ),
        })
    }

    fn end_turn(&mut self) -> Result<()> {
        self.turn_actions.clear();

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
                if let Ok(player) = self.active_player() {
                    let gains_gold = player.gold == 0 && player.city_has(DistrictName::PoorHouse);
                    let gains_cards = player.hand.len() == 0 && player.city_has(DistrictName::Park);

                    if gains_gold {
                        self.active_player_mut().unwrap().gold += 1;
                        self.logs.push(Log {
                            display: format!(
                                "{} gains 1 gold from their Poor House.",
                                self.active_player().unwrap().name,
                            ),
                            source: Source::Triggered,
                        });
                    }

                    if gains_cards {
                        self.gain_cards(2);
                        self.logs.push(Log {
                            display: format!(
                                "{} gains 2 cards from their Park.",
                                self.active_player().unwrap().name,
                            ),
                            source: Source::Triggered,
                        });
                    }
                }

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
        // triggered actions
        let rank = 4;
        let character = &self.characters[rank as usize - 1];
        if character.markers.iter().any(|m| *m == Marker::Assassinated) {
            if let Some((player, role)) = self.players.iter().find_map(|p| {
                p.roles
                    .iter()
                    .find(|role| role.rank() == rank)
                    .map(|role| (p, role))
            }) {
                if *role == RoleName::King || *role == RoleName::Patrician {
                    self.crowned = player.name.clone();
                    self.logs.push(Log {
                        source: Source::Triggered,
                        display: format!("{}'s heir {} crowned.", role.display_name(), player.name),
                    });
                }
                if *role == RoleName::Emperor {
                    todo!();
                }
            }
        }

        // cleanup
        for character in self.characters.iter_mut() {
            character.markers.clear();
        }
        self.draft.clear();
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
