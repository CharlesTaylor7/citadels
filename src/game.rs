use crate::actions::{
    Action, ActionTag, BuildMethod, CityDistrictTarget, MagicianAction, Resource, WizardMethod,
};
use crate::districts::DistrictName;
use crate::lobby::{self, Lobby};
use crate::museum::Museum;
use crate::random::Prng;
use crate::roles::{Rank, RoleName};
use crate::sqlite::DbLog;
use crate::types::{CardSuit, Marker, PlayerId, PlayerName};
use macros::tag::Tag;
use rand::prelude::*;
use std::borrow::{Borrow, BorrowMut, Cow};
use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::repeat;
use std::mem;

#[derive(Debug)]
pub enum ForcedToGatherReason {
    Witch,
    Bewitched,
    Blackmailed,
}

pub type Result<T> = std::result::Result<T, Cow<'static, str>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct PlayerIndex(pub usize);

#[derive(Debug)]
pub struct Player {
    pub index: PlayerIndex,
    pub id: PlayerId,
    pub name: PlayerName,
    pub gold: usize,
    pub hand: Vec<DistrictName>,
    pub city: Vec<CityDistrict>,
    pub roles: Vec<RoleName>,
}

impl Player {
    pub fn city_size(&self) -> usize {
        self.city
            .iter()
            .map(|d| {
                if d.name == DistrictName::Monument {
                    2
                } else {
                    1
                }
            })
            .sum()
    }
    pub fn count_suit_for_resource_gain(&self, suit: CardSuit) -> usize {
        self.city
            .iter()
            .filter(|c| c.name.data().suit == suit || c.name == DistrictName::SchoolOfMagic)
            .count()
    }

    pub fn cleanup_round(&mut self) {
        self.roles.clear();
    }

    pub fn city_has(&self, name: DistrictName) -> bool {
        self.city.iter().any(|c| c.name == name)
    }

    pub fn has_role(&self, name: RoleName) -> bool {
        self.roles.iter().any(|c| *c == name)
    }

    pub fn new(index: PlayerIndex, id: String, name: PlayerName) -> Self {
        Player {
            index,
            id,
            name,
            gold: 2,
            hand: Vec::new(),
            city: Vec::new(),
            roles: Vec::with_capacity(2),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CityDistrict {
    pub name: DistrictName,
    pub beautified: bool,
}

impl CityDistrict {
    pub fn effective_cost(&self) -> usize {
        let mut cost = self.name.data().cost;
        if self.beautified {
            cost += 1;
        }
        cost
    }
    pub fn from(name: DistrictName) -> Self {
        Self {
            name,
            beautified: false,
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
    pub fn shuffle<R: RngCore>(&mut self, rng: &mut R) {
        self.deck.append(&mut self.discard);
        self.deck.shuffle(rng);
    }

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

#[derive(Debug)]
pub enum Turn {
    GameOver,
    Draft(Draft),
    Call(Call),
}

impl Turn {
    pub fn call(&self) -> Result<&Call> {
        if let Turn::Call(call) = self {
            Ok(call)
        } else {
            Err("not the call phase".into())
        }
    }

    pub fn draft(&self) -> Result<&Draft> {
        if let Turn::Draft(draft) = self {
            Ok(draft)
        } else {
            Err("not the draft".into())
        }
    }

    pub fn draft_mut(&mut self) -> Result<&mut Draft> {
        if let Turn::Draft(draft) = self {
            Ok(draft)
        } else {
            Err("not the draft".into())
        }
    }
}

#[derive(Debug)]
pub struct Call {
    pub rank: Rank,
    pub end_of_round: bool,
}

#[derive(Debug)]
pub struct Draft {
    pub player_count: usize,
    pub player: PlayerIndex,
    pub theater_step: bool,
    pub remaining: Vec<RoleName>,
    pub initial_discard: Option<RoleName>,
    pub faceup_discard: Vec<RoleName>,
}

impl Draft {
    pub fn begin<R: RngCore>(
        player_count: usize,
        player: PlayerIndex,
        roles: Vec<RoleName>,
        rng: &mut R,
    ) -> Self {
        let mut draft = Self {
            player_count,
            player,
            remaining: roles,
            theater_step: false,
            initial_discard: None,
            faceup_discard: vec![],
        };
        let role_count = draft.remaining.len();
        // discard cards face up in 4+ player game
        if draft.player_count >= 4 {
            for _ in draft.player_count + 2..role_count {
                let mut index;
                loop {
                    index = rng.gen_range(0..draft.remaining.len());
                    if draft.remaining[index].can_be_discarded_faceup() {
                        break;
                    }
                }

                draft
                    .faceup_discard
                    .push(draft.remaining.swap_remove(index));
            }
        }

        // discard 1 card facedown
        let i = rng.gen_range(0..draft.remaining.len());
        draft.initial_discard = Some(draft.remaining.swap_remove(i));

        // restore sort of roles
        draft.remaining.sort_by_key(|role| role.rank());
        draft
    }

    pub fn clear(&mut self) {
        self.remaining.clear();
        self.initial_discard = None;
        self.faceup_discard.clear();
    }
}

#[derive(Debug)]
pub struct GameRole {
    pub role: RoleName,
    pub markers: Vec<Marker>,
    pub player: Option<PlayerIndex>,
    pub revealed: bool,
    pub logs: Vec<Cow<'static, str>>,
}

impl Default for GameRole {
    fn default() -> Self {
        Self {
            role: RoleName::Spy,
            markers: Vec::with_capacity(0),
            revealed: false,
            player: None,
            logs: Vec::with_capacity(0),
        }
    }
}

impl GameRole {
    pub fn has_blackmail(&self) -> bool {
        self.markers.iter().any(|m| {
            if let Marker::Blackmail { .. } = m {
                true
            } else {
                false
            }
        })
    }

    pub fn has_warrant(&self) -> bool {
        self.markers.iter().any(|m| {
            if let Marker::Warrant { .. } = m {
                true
            } else {
                false
            }
        })
    }
    pub fn cleanup_round(&mut self) {
        self.markers.clear();
        self.player = None;
        self.revealed = false;
        self.logs.clear();
    }
}

pub type ActionResult = Result<ActionOutput>;

pub struct ActionOutput {
    pub log: Cow<'static, str>,
    pub followup: Option<Followup>,
    pub end_turn: bool,
    pub notifications: Vec<()>,
}
impl ActionOutput {
    pub fn new(log: impl Into<Cow<'static, str>>) -> Self {
        Self {
            log: log.into(),
            followup: None,
            end_turn: false,
            notifications: vec![],
        }
    }

    pub fn end_turn(mut self) -> Self {
        self.end_turn = true;
        self
    }

    pub fn followup(mut self, followup: Followup) -> Self {
        self.followup = Some(followup);
        self
    }

    pub fn maybe_followup(mut self, followup: Option<Followup>) -> Self {
        self.followup = followup;
        self
    }

    pub fn notify(mut self, notification: ()) -> Self {
        self.notifications.push(notification);
        self
    }
}

#[derive(Debug)]
pub enum Followup {
    Bewitch,
    GatherCardsPick {
        revealed: Vec<DistrictName>,
    },
    ScholarPick {
        revealed: Vec<DistrictName>,
    },
    WizardPick {
        player: PlayerIndex,
    },
    SeerDistribute {
        players: Vec<PlayerIndex>,
    },
    SpyAcknowledge {
        player: PlayerName,
        revealed: Vec<DistrictName>,
    },
    Warrant {
        signed: bool,
        magistrate: PlayerIndex,
        gold: usize,
        district: DistrictName,
    },
    Blackmail {
        blackmailer: PlayerIndex,
    },
    HandleBlackmail,
}

impl Followup {
    pub fn actions(&self) -> Vec<ActionTag> {
        match self {
            Self::Bewitch { .. } => vec![ActionTag::Bewitch],
            Self::HandleBlackmail { .. } => vec![ActionTag::PayBribe, ActionTag::IgnoreBlackmail],
            Self::SpyAcknowledge { .. } => vec![ActionTag::SpyAcknowledge],
            Self::GatherCardsPick { .. } => vec![ActionTag::GatherCardsPick],
            Self::ScholarPick { .. } => vec![ActionTag::ScholarPick],
            Self::WizardPick { .. } => vec![ActionTag::WizardPick],
            Self::SeerDistribute { .. } => vec![ActionTag::SeerDistribute],
            Self::Blackmail { .. } => vec![ActionTag::RevealBlackmail, ActionTag::Pass],
            Self::Warrant { signed, .. } => {
                if *signed {
                    vec![ActionTag::RevealWarrant, ActionTag::Pass]
                } else {
                    vec![ActionTag::Pass]
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Game {
    rng: Prng,
    // global
    pub round: usize,
    pub deck: Deck<DistrictName>,
    pub players: Vec<Player>,
    pub characters: Characters,
    pub crowned: PlayerIndex,
    pub first_to_complete: Option<PlayerIndex>,

    // current turn info
    pub active_turn: Turn,
    pub followup: Option<Followup>,
    pub turn_actions: Vec<Action>,
    pub remaining_builds: usize,

    // logs
    pub logs: Vec<Cow<'static, str>>,
    pub db_log: Option<DbLog>,

    // card specific metadata
    pub museum: Museum,
    pub alchemist: usize,
    pub tax_collector: usize,
}

#[derive(Debug)]
pub struct Characters(pub Vec<GameRole>);

impl Characters {
    pub fn next(&self, rank: Rank) -> Option<Rank> {
        self.0.last().and_then(|c| {
            rank.next()
                .and_then(|r| if r <= c.role.rank() { Some(r) } else { None })
        })
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter_c(&self) -> impl Iterator<Item = &GameRole> + '_ {
        self.0.iter()
    }

    pub fn iter(&self) -> impl Iterator<Item = RoleName> + '_ {
        self.0.iter().map(|c| c.role)
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut GameRole> + '_ {
        self.0.iter_mut()
    }

    pub fn new(roles: &Vec<RoleName>) -> Self {
        Self(
            roles
                .iter()
                .map(|role| GameRole {
                    role: *role,
                    player: None,
                    revealed: false,
                    markers: vec![],
                    logs: vec![],
                })
                .collect(),
        )
    }
    pub fn get(&self, rank: Rank) -> &GameRole {
        &self.0[rank.to_index()]
    }

    pub fn get_mut(&mut self, rank: Rank) -> &mut GameRole {
        &mut self.0[rank.to_index()]
    }

    pub fn has_bishop_protection(&self, player: PlayerIndex) -> bool {
        let bishop = self.get(Rank::Five);
        if bishop.role != RoleName::Bishop {
            return false;
        }
        if !bishop.revealed {
            return false;
        }

        if bishop.markers.iter().any(|m| *m == Marker::Bewitched) {
            return self.get(Rank::One).player.is_some_and(|w| w == player);
        }

        bishop.player.is_some_and(|p| p == player)
    }

    pub fn has_tax_collector(&self) -> bool {
        self.0.get(Rank::Nine.to_index()).map(|c| c.role) == Some(RoleName::TaxCollector)
    }
}

impl Game {
    pub fn complete_city_size(&self) -> usize {
        if self.players.len() <= 3 {
            8
        } else {
            7
        }
    }

    pub fn total_score(&self, player: &Player) -> usize {
        let mut score = self.public_score(player);

        for card in &player.hand {
            if *card == DistrictName::SecretVault {
                score += 3;
            }
        }
        score
    }

    pub fn public_score(&self, player: &Player) -> usize {
        if player.city_has(DistrictName::HauntedQuarter) {
            [
                CardSuit::Religious,
                CardSuit::Military,
                CardSuit::Trade,
                CardSuit::Noble,
                CardSuit::Unique,
            ]
            .iter()
            .map(|s| self.public_score_impl(player, Some(*s)))
            .max()
            .expect("Suit array is not empty")
        } else {
            self.public_score_impl(player, None)
        }
    }
    fn public_score_impl(&self, player: &Player, haunted: Option<CardSuit>) -> usize {
        let mut score = 0;
        let mut counts: [usize; 5] = [0, 0, 0, 0, 0];

        if let Some(suit) = haunted {
            counts[suit as usize] += 1;
        }

        // total costs
        for card in &player.city {
            if card.name != DistrictName::SecretVault {
                score += card.effective_cost();
            }
            if card.name != DistrictName::HauntedQuarter {
                counts[card.name.data().suit as usize] += 1;
            }
        }

        // uniques
        for card in &player.city {
            score += match card.name {
                DistrictName::DragonGate => 2,
                DistrictName::MapRoom => player.hand.len(),
                DistrictName::ImperialTreasury => player.gold,
                DistrictName::Statue if player.index == self.crowned => 5,
                DistrictName::Capitol if counts.iter().any(|c| *c >= 3) => 3,
                DistrictName::IvoryTower if 1 == counts[CardSuit::Unique as usize] => 5,
                DistrictName::WishingWell => counts[CardSuit::Unique as usize],
                DistrictName::Museum => self.museum.cards().len(),
                DistrictName::Basilica => player
                    .city
                    .iter()
                    .filter(|c| c.effective_cost() % 2 == 1)
                    .count(),

                _ => 0,
            }
        }

        // one district of each type: 3 points
        if counts.iter().all(|s| *s > 0) {
            score += 3;
        }

        // first_to_complete: 4
        if self
            .first_to_complete
            .as_ref()
            .is_some_and(|c| *c == player.index)
        {
            score += 4;
        }
        // other completed: 2
        else if player.city_size() >= self.complete_city_size() {
            score += 2;
        }

        score
    }

    pub fn active_role(&self) -> Result<&GameRole> {
        let call = self.active_turn.call()?;
        Ok(self.characters.get(call.rank))
    }

    pub fn active_role_mut(&mut self) -> Result<&mut GameRole> {
        let call = self.active_turn.call()?;
        Ok(self.characters.get_mut(call.rank))
    }

    pub fn start(lobby: Lobby, mut rng: Prng) -> Result<Game> {
        let Lobby {
            mut players,
            config,
        } = lobby;

        let db_log = DbLog::new(rng.seed, &players)
            .map_err(|e| log::error!("{}", e))
            .ok();

        // randomize the seating order
        players.shuffle(&mut rng);

        // create players from the lobby, and filter players who were kicked
        let mut players: Vec<_> = players
            .into_iter()
            .enumerate()
            .map(|(index, lobby::Player { id, name })| Player::new(PlayerIndex(index), id, name))
            .collect();

        let mut deck: Vec<DistrictName> = crate::districts::NORMAL
            .iter()
            .flat_map(|district| repeat(district.name).take(district.name.multiplicity()))
            .chain(config.select_unique_districts(&mut rng))
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

        let characters = Characters::new(&config.select_roles(&mut rng, players.len())?);
        let crowned = PlayerIndex(0);
        let mut game = Game {
            rng,
            players,
            db_log,
            crowned,
            characters,
            round: 0,
            alchemist: 0,
            tax_collector: 0,
            deck: Deck::new(deck),
            active_turn: Turn::GameOver,
            turn_actions: Vec::new(),
            remaining_builds: 0,
            logs: Vec::new(),
            followup: None,
            museum: Museum::default(),
            first_to_complete: None,
        };

        if !cfg!(feature = "dev") {
            game.begin_draft();
            Ok(game)
        } else {
            let test_role = RoleName::Witch;
            // deal roles out randomly
            game.characters.get_mut(test_role.rank()).role = test_role;
            game.characters.get_mut(Rank::Three).role = RoleName::Wizard;
            let mut roles: Vec<_> = game.characters.iter().collect();
            roles.shuffle(&mut game.rng);

            let role_count = if game.players.len() <= 3 { 2 } else { 1 };

            let roles = roles.iter().enumerate();
            // .take(role_count * game.players.len());
            for (i, role) in roles {
                let index = i % game.players.len();
                game.players[index].roles.push(*role);
                game.characters.get_mut(role.rank()).player = Some(PlayerIndex(index));
            }

            for p in game.players.iter_mut() {
                p.roles.sort_by_key(|c| c.rank());
                p.hand = vec![
                    DistrictName::ThievesDen,
                    DistrictName::Framework,
                    DistrictName::Necropolis,
                    DistrictName::Temple,
                ];
                p.gold = 3;

                for card in game.deck.draw_many(3) {
                    p.city.push(CityDistrict {
                        name: card,
                        beautified: false,
                    });
                }
                p.city.push(CityDistrict {
                    name: DistrictName::Framework,
                    beautified: false,
                });
            }

            game.active_turn = Turn::Call(Call {
                rank: test_role.rank(),
                end_of_round: false,
            });
            game.start_turn().unwrap();
            Ok(game)
        }
    }

    pub fn begin_draft(&mut self) {
        self.round += 1;
        let draft = Draft::begin(
            self.players.len(),
            self.crowned,
            self.characters.iter().collect(),
            &mut self.rng,
        );
        for c in draft.faceup_discard.iter() {
            self.characters
                .get_mut(c.rank())
                .markers
                .push(Marker::Discarded);
        }
        self.active_turn = Turn::Draft(draft);
    }

    pub fn responding_player_index(&self) -> Result<PlayerIndex> {
        if let Some(o) = self.followup.as_ref() {
            return match o {
                Followup::Warrant { magistrate, .. } => Ok(*magistrate),
                Followup::Blackmail { blackmailer, .. } => Ok(*blackmailer),
                Followup::Bewitch { .. } => self.active_player_index(),
                Followup::HandleBlackmail { .. } => self.active_player_index(),
                Followup::SpyAcknowledge { .. } => self.active_player_index(),
                Followup::WizardPick { .. } => self.active_player_index(),
                Followup::SeerDistribute { .. } => self.active_player_index(),
                Followup::ScholarPick { .. } => self.active_player_index(),
                Followup::GatherCardsPick { .. } => self.active_player_index(),
            };
        }
        Err("No pending response".into())
    }

    pub fn responding_player(&self) -> Result<&Player> {
        self.responding_player_index()
            .map(|i| self.players[i.0].borrow())
    }

    pub fn active_player_index(&self) -> Result<PlayerIndex> {
        match &self.active_turn {
            Turn::GameOver => Err("game over".into()),
            Turn::Draft(draft) => Ok(draft.player),
            Turn::Call(call) => {
                let c = self.characters.get(call.rank);
                if self.has_gathered_resources()
                    && c.markers.iter().any(|m| *m == Marker::Bewitched)
                {
                    self.characters
                        .get(Rank::One)
                        .player
                        .ok_or("no witch!".into())
                } else {
                    c.player
                        .ok_or(format!("no player with rank {}", call.rank).into())
                }
            }
        }
    }

    pub fn active_player(&self) -> Result<&Player> {
        self.active_player_index()
            .map(|i| self.players[i.0].borrow())
    }

    pub fn active_player_mut(&mut self) -> Result<&mut Player> {
        self.active_player_index()
            .map(|i| self.players[i.0].borrow_mut())
    }

    pub fn active_perform_count(&self, action: ActionTag) -> usize {
        self.turn_actions
            .iter()
            .filter(|act| act.tag() == action)
            .count()
    }

    pub fn allowed_for(&self, id: Option<&str>) -> Vec<ActionTag> {
        let id = if let Some(id) = id { id } else { return vec![] };
        if let Ok(p) = self.responding_player() {
            if p.id == id {
                self.followup.as_ref().unwrap().actions()
            } else {
                vec![]
            }
        } else if self.active_player().is_ok_and(|p| p.id == id) {
            self.active_player_actions()
        } else {
            vec![]
        }
    }

    pub fn has_gathered_resources(&self) -> bool {
        let followup = self.followup.as_ref().is_some_and(|a| {
            if let Followup::GatherCardsPick { .. } = a {
                true
            } else {
                false
            }
        });
        !followup
            && self
                .turn_actions
                .iter()
                .any(|act| act.tag().is_resource_gathering())
    }

    pub fn forced_to_gather_resources(&self) -> Option<ForcedToGatherReason> {
        if self.has_gathered_resources() {
            return None;
        }
        self.active_role().ok().and_then(|c| {
            if c.role == RoleName::Witch {
                Some(ForcedToGatherReason::Witch)
            } else if c.markers.iter().any(|m| *m == Marker::Bewitched) {
                Some(ForcedToGatherReason::Bewitched)
            } else if c.markers.iter().any(|m| m.is_blackmail()) {
                Some(ForcedToGatherReason::Blackmailed)
            } else {
                None
            }
        })
    }

    pub fn active_player_actions(&self) -> Vec<ActionTag> {
        match self.active_turn.borrow() {
            Turn::GameOver => {
                vec![]
            }
            Turn::Draft(Draft {
                theater_step: true, ..
            }) => {
                if self.turn_actions.iter().any(|act| {
                    act.tag() == ActionTag::Theater || act.tag() == ActionTag::TheaterPass
                }) {
                    vec![]
                } else {
                    vec![ActionTag::Theater, ActionTag::TheaterPass]
                }
            }
            Turn::Draft(_draft) => {
                if self.active_perform_count(ActionTag::DraftPick) == 0 {
                    vec![ActionTag::DraftPick]
                } else {
                    vec![ActionTag::DraftDiscard]
                }
                //else { vec![] }
            }

            Turn::Call(Call {
                end_of_round: true, ..
            }) => {
                if self.active_role().unwrap().role != RoleName::Emperor {
                    log::error!("What are you doing man?");
                    vec![]
                } else if self.active_perform_count(ActionTag::EmperorHeirGiveCrown) == 0 {
                    vec![ActionTag::EmperorHeirGiveCrown]
                } else {
                    vec![]
                }
            }

            Turn::Call(call) => {
                let player = if let Ok(player) = self.active_player() {
                    player
                } else {
                    return vec![];
                };

                if self.forced_to_gather_resources().is_some() {
                    return vec![
                        ActionTag::GatherResourceGold,
                        ActionTag::GatherResourceCards,
                    ];
                }

                let mut actions = Vec::new();
                let c = self.characters.get(call.rank);
                for (n, action) in c.role.data().actions {
                    if self.active_perform_count(*action) < *n {
                        actions.push(*action);
                    }
                }

                for card in player.city.iter() {
                    if let Some(action) = card.name.action() {
                        if self.active_perform_count(action) < 1 {
                            actions.push(action);
                        }
                    }
                }

                // You have to gather resources before building
                if !self.has_gathered_resources() {
                    // gather
                    actions.push(ActionTag::GatherResourceGold);
                    actions.push(ActionTag::GatherResourceCards);
                } else if self.active_role().unwrap().role != RoleName::Navigator {
                    // build
                    actions.push(ActionTag::Build);
                }

                if actions.iter().all(|action| !action.is_required()) {
                    actions.push(ActionTag::EndTurn);
                }

                actions
            }
        }
    }

    pub fn perform(&mut self, action: Action, id: &str) -> Result<()> {
        if !self.allowed_for(Some(id)).contains(&action.tag()) {
            return Err("not allowed".into());
        }

        let ActionOutput {
            followup,
            log,
            end_turn,
            notifications: _,
        } = self.perform_action(&action)?;

        if let Some(log) = self.db_log.as_mut() {
            if let Err(err) = log.append(&action) {
                log::error!("{}", err);
                log::info!("Disabling db action log");
                self.db_log = None;
            }
        }

        self.followup = followup;

        log::info!("{:#?}", log);
        log::info!("followup: {:#?}", self.followup);

        self.turn_actions.push(action.clone());
        if let Ok(role) = self.active_role_mut() {
            role.logs.push(log.into());
        }

        if end_turn {
            self.end_turn()?;
        }

        Ok(())
    }

    fn start_turn(&mut self) -> Result<()> {
        if self.active_turn.call().is_ok_and(|call| call.end_of_round) {
            return Ok(());
        }

        let c = self.active_role_mut();
        if c.is_err() {
            return Ok(());
        }
        let c_ref = c.unwrap();

        let mut c = std::mem::replace(c_ref, GameRole::default());

        log::info!("Calling {}", c.role.display_name());
        if c.markers.iter().any(|m| *m == Marker::Killed) {
            c.logs.push("They were killed!".into());

            *c_ref = c;
            self.call_next();
            return self.start_turn();
        }

        if c.player.is_none() {
            c.logs.push("No one responds".into());

            *c_ref = c;
            self.call_next();
            return self.start_turn();
        }
        c.revealed = true;
        self.remaining_builds = c.role.build_limit();

        let player = self.players[c.player.unwrap().0].borrow();
        c.logs
            .push(format!("{} starts their turn.", player.name).into());

        if c.markers.iter().any(|m| *m == Marker::Bewitched) {
            c.logs.push(
                format!(
                    "They are bewitched! After gathering resources, their turn will be yielded to the Witch ({}).",
                    self.players[self.characters.get(RoleName::Witch.rank()).player.unwrap().0].name
                )
                .into(),
            );
        }

        if c.markers.iter().any(|m| *m == Marker::Robbed) {
            let player = self.players[c.player.unwrap().0].borrow_mut();
            let gold = player.gold;
            player.gold = 0;
            let thief = self.characters.get(RoleName::Thief.rank()).player.unwrap();

            let thief = &mut self.players[thief.0];
            thief.gold += gold;
            c.logs.push(
                format!(
                    "The Thief ({}) takes all {} of their gold!",
                    thief.name, gold
                )
                .into(),
            );
        }

        *self.active_role_mut().unwrap() = c;

        Ok(())
    }

    fn discard_district(&mut self, district: DistrictName) {
        if district == DistrictName::Museum {
            let museum = std::mem::replace(&mut self.museum, Museum::default());
            let mut to_discard = museum.into_cards();
            to_discard.push(DistrictName::Museum);
            to_discard.shuffle(&mut self.rng);
            for card in to_discard {
                self.deck.discard_to_bottom(card);
            }
        } else {
            self.deck.discard_to_bottom(district);
        }
    }

    fn complete_build(&mut self, player: PlayerIndex, spent: usize, district: DistrictName) {
        let player = self.players[player.0].borrow_mut();

        player.city.push(CityDistrict::from(district));
        if self.active_role().unwrap().role == RoleName::Alchemist {
            self.alchemist += spent;
        }
        self.check_city_for_completion();
    }

    fn check_city_for_completion(&mut self) {
        let player = self.active_player().unwrap();
        if player.city_size() >= self.complete_city_size() && self.first_to_complete.is_none() {
            log::info!("{} is the first to complete their city", player.name);
            self.first_to_complete = Some(player.index);
        }
    }
    fn after_gather_resources(&self) -> Option<Followup> {
        log::info!("After gathering, is there a forced followup?");
        self.forced_to_gather_resources()
            .and_then(|reason| match reason {
                ForcedToGatherReason::Witch => Some(Followup::Bewitch),
                ForcedToGatherReason::Bewitched => None,
                ForcedToGatherReason::Blackmailed => Some(Followup::HandleBlackmail),
            })
    }

    fn perform_action(&mut self, action: &Action) -> ActionResult {
        Ok(match action {
            Action::RevealWarrant => match self.followup {
                Some(Followup::Warrant {
                    magistrate,
                    gold,
                    district,
                    signed,
                }) => {
                    if !signed {
                        return Err("Cannot reveal unsigned warrant".into());
                    }
                    if self.players[magistrate.0].city_has(district) {
                        return Err("Cannot confiscate a district you already have.".into());
                    }
                    let player = self.active_player_mut().unwrap();
                    player.gold += gold;
                    let magistrate = self.players[magistrate.0].borrow_mut();
                    magistrate.city.push(CityDistrict::from(district));
                    let name = magistrate.name.clone();

                    // clear all remaining warrants
                    for c in self.characters.iter_mut() {
                        if let Some((i, _)) =
                            c.markers.iter().enumerate().find(|(_, m)| m.is_warrant())
                        {
                            c.markers.remove(i);
                        }
                    }

                    ActionOutput::new(format!(
                            "The Magistrate ({}) reveals a signed warrant and confiscates the {}; {} gold is refunded.",
                            name,
                            district.data().display_name, 
                            gold
                        )
                    )
                }
                _ => return Err("cannot reveal warrant".into()),
            },

            Action::PayBribe => {
                let player = self.active_player_mut().unwrap();
                let half = player.gold / 2;
                player.gold -= half;

                let blackmailer = self.characters.get(Rank::Two).player.unwrap();
                self.players[blackmailer.0].gold += half;

                ActionOutput::new(format!(
                    "They bribed the Blackmailer ({}) with {} gold.",
                    self.players[blackmailer.0].name, half
                ))
            }

            Action::IgnoreBlackmail => {
                //
                ActionOutput::new(
                    "They ignored the blackmail. Waiting on the Blackmailer's response.",
                )
                .followup(Followup::Blackmail {
                    blackmailer: self.characters.get(Rank::Two).player.unwrap(),
                })
            }

            Action::RevealBlackmail => match self.followup {
                Some(Followup::Blackmail { blackmailer }) => {
                    let is_flowered = self
                        .active_role()?
                        .markers
                        .iter()
                        .any(|marker| *marker == Marker::Blackmail { flowered: true });

                    if is_flowered {
                        let target = self.active_player_mut().unwrap();
                        let gold = std::mem::replace(&mut target.gold, 0);
                        self.players[blackmailer.0].gold += gold;

                        // clear all remaining blackmail
                        for c in self.characters.iter_mut() {
                            if let Some((i, _)) =
                                c.markers.iter().enumerate().find(|(_, m)| m.is_blackmail())
                            {
                                c.markers.remove(i);
                            }
                        }

                        ActionOutput::new(format!(
                            "The Blackmailer ({}) reveals an active threat, and takes all {} of their gold.", 
                            self.players[blackmailer.0].name,
                            gold 
                        ))
                    } else {
                        let name = self.active_player().unwrap().name.clone();
                        ActionOutput::new(format!(
                            "The Blackmailer ({}) reveals an empty threat. Nothing happens.",
                            name
                        ))
                    }
                }
                _ => return Err("Cannot reveal blackmail".into()),
            },

            Action::Pass => {
                //
                match self.followup {
                    Some(Followup::Warrant {
                        gold,
                        district,
                        magistrate,
                        ..
                    }) => {
                        //
                        self.complete_build(self.active_player_index()?, gold, district);
                        ActionOutput::new(format!(
                            "The Magistrate ({}) did not reveal the warrant.",
                            self.players[magistrate.0].name
                        ))
                    }
                    Some(Followup::Blackmail { blackmailer }) => {
                        //
                        ActionOutput::new(format!(
                            "The Blackmailer ({}) did not reveal the blackmail.",
                            self.players[blackmailer.0].name,
                        ))
                    }
                    _ => return Err("impossible".into()),
                }
            }

            Action::DraftPick { role } => {
                let draft = self.active_turn.draft_mut()?;

                Game::remove_first(&mut draft.remaining, *role);
                let c = self.characters.get_mut(role.rank());
                c.player = Some(draft.player);
                let player = self.players[draft.player.0].borrow_mut();
                player.roles.push(*role);
                player.roles.sort_by_key(|r| r.rank());

                let output = ActionOutput::new(format!("{} drafts a role.", player.name));

                // the first player to draft in the two player game does not discard.
                // the last pick is between two cards.
                // the one they don't pick is automatically discarded.
                // So really only the middle two draft turns should show the discard button
                if self.players.len() == 2
                    && (draft.remaining.len() == 5 || draft.remaining.len() == 3)
                {
                    output
                } else {
                    output.end_turn()
                }
            }

            Action::DraftDiscard { role } => {
                let draft = self.active_turn.draft_mut()?;
                let i = (0..draft.remaining.len())
                    .find(|i| draft.remaining[*i] == *role)
                    .ok_or("selected role is not available")?;

                draft.remaining.remove(i);
                ActionOutput::new(format!(
                    "{} discards a role face down.",
                    self.active_player()?.name
                ))
                .end_turn()
            }

            Action::EndTurn => {
                //
                ActionOutput::new(format!("{} ends their turn.", self.active_player()?.name))
                    .end_turn()
            }

            Action::GatherResourceGold => {
                let active = self.active_player_index()?;
                let mut amount = 2;
                let log = if self.players[active.0].city_has(DistrictName::GoldMine) {
                    amount += 1;
                    format!(
                        "{} gathers 3 gold. (1 extra from their Gold Mine).",
                        self.players[active.0].name
                    )
                } else {
                    format!("{} gathers 2 gold.", self.players[active.0].name)
                };

                self.players[active.0].gold += amount;

                ActionOutput::new(log).maybe_followup(self.after_gather_resources())
            }

            Action::GatherResourceCards => {
                let mut draw_amount = 2;
                if self.active_player()?.city_has(DistrictName::Observatory) {
                    draw_amount += 1;
                }

                let mut drawn = self.deck.draw_many(draw_amount).collect();

                if self.active_player()?.city_has(DistrictName::Library) {
                    self.active_player_mut()?.hand.append(&mut drawn);

                    ActionOutput::new(format!(
                        "{} gathers cards. With the aid of their library they keep all {} cards.",
                        self.active_player()?.name,
                        draw_amount
                    ))
                    .maybe_followup(self.after_gather_resources())
                } else {
                    let followup = if drawn.len() > 0 {
                        Some(Followup::GatherCardsPick { revealed: drawn })
                    } else {
                        self.after_gather_resources()
                    };
                    ActionOutput::new(format!(
                        "{} reveals {} cards from the top of the deck.",
                        self.active_player()?.name,
                        draw_amount
                    ))
                    .maybe_followup(followup)
                }
            }

            Action::GatherCardsPick { district } => {
                let mut revealed = if let Some(Followup::GatherCardsPick { revealed, .. }) =
                    self.followup.borrow_mut()
                {
                    revealed
                } else {
                    return Err("action is not allowed".into());
                };

                Game::remove_first(&mut revealed, *district).ok_or("invalid choice")?;
                revealed.shuffle(&mut self.rng);

                for remaining in revealed {
                    self.deck.discard_to_bottom(*remaining);
                }
                self.active_player_mut()?.hand.push(*district);
                ActionOutput::new("They pick a card.").maybe_followup(self.after_gather_resources())
            }
            Action::GoldFromNobility => self.gain_gold_for_suit(CardSuit::Noble)?,
            Action::GoldFromReligion => self.gain_gold_for_suit(CardSuit::Religious)?,
            Action::GoldFromTrade => self.gain_gold_for_suit(CardSuit::Trade)?,
            Action::GoldFromMilitary => self.gain_gold_for_suit(CardSuit::Military)?,

            Action::CardsFromNobility => self.gain_cards_for_suit(CardSuit::Noble)?,
            Action::CardsFromReligion => self.gain_cards_for_suit(CardSuit::Religious)?,

            Action::MerchantGainOneGold => {
                let player = self.active_player_mut()?;
                player.gold += 1;
                ActionOutput::new(format!(
                    "The Merchant ({}) gains 1 extra gold.",
                    player.name
                ))
            }
            Action::ArchitectGainCards => {
                self.gain_cards(2);
                let player = self.active_player()?;
                ActionOutput::new(format!(
                    "The Architect ({}) gains 2 extra cards.",
                    player.name
                ))
            }

            Action::Build(method) => {
                if self.active_role().unwrap().role == RoleName::Navigator {
                    Err("The navigator is not allowed to build.")?;
                }
                let district = match method {
                    BuildMethod::Regular { district } => *district,
                    BuildMethod::Cardinal { district, .. } => *district,
                    BuildMethod::Framework { district } => *district,
                    BuildMethod::ThievesDen { .. } => DistrictName::ThievesDen,
                    BuildMethod::Necropolis { .. } => DistrictName::Necropolis,
                };

                let active = self.active_player()?;
                if active.hand.iter().all(|d| *d != district) {
                    Err("Card not in hand")?;
                }

                if self
                    .turn_actions
                    .iter()
                    .all(|a| !a.tag().is_resource_gathering())
                {
                    Err("Must gather resources before building")?;
                }

                let is_free_build = district == DistrictName::Stables
                    || (district.data().suit == CardSuit::Trade
                        && self.active_role().unwrap().role == RoleName::Trader);

                if !is_free_build && self.remaining_builds == 0 {
                    Err(format!(
                        "With your role, you cannot build more than {} time(s), this turn.",
                        self.active_role().unwrap().role.build_limit()
                    ))?;
                }

                if !(active.city_has(DistrictName::Quarry)
                    || self.active_role().unwrap().role == RoleName::Wizard)
                    && active.city.iter().any(|d| d.name == district)
                {
                    return Err("cannot build duplicate".into());
                }

                if district == DistrictName::Monument && active.city.len() >= 5 {
                    return Err("You can only build the Monument, if you have less than 5 districts in your city".into());
                }

                let district = district.data();

                let mut cost = district.cost;
                if district.suit == CardSuit::Unique
                    && active.city.iter().any(|d| d.name == DistrictName::Factory)
                {
                    cost -= 1;
                }

                match method {
                    BuildMethod::Regular { .. } => {
                        if cost > active.gold {
                            Err("Not enough gold")?;
                        }

                        let active = self.active_player_mut()?;
                        Game::remove_first(&mut active.hand, district.name)
                            .ok_or("card not in hand")?;
                        active.gold -= cost;
                    }
                    BuildMethod::Cardinal {
                        discard, player, ..
                    } => {
                        if self.active_role()?.role != RoleName::Cardinal {
                            Err("You are not the cardinal")?;
                        }
                        if active.gold + discard.len() < cost {
                            Err("Not enough gold or discarded")?;
                        }

                        if active.gold + discard.len() > cost {
                            Err("Must spend own gold first, before taking from others")?;
                        }

                        let target = self
                            .players
                            .iter()
                            .find_map(|p| {
                                if p.name == *player {
                                    Some(p.index)
                                } else {
                                    None
                                }
                            })
                            .ok_or("Player does not exist")?;

                        if self.players[target.0].gold < discard.len() {
                            Err("Cannot give more cards than the target has gold")?;
                        }

                        cost -= discard.len();
                        let mut discard = discard.clone();
                        let mut copy = discard.clone();
                        let mut new_hand = Vec::with_capacity(active.hand.len());
                        for card in active.hand.iter() {
                            if let Some((i, _)) =
                                discard.iter().enumerate().find(|(_, d)| *d == card)
                            {
                                discard.swap_remove(i);
                            } else {
                                new_hand.push(*card);
                            }
                        }

                        if discard.len() > 0 {
                            Err("Can't discard cards not in your hand")?;
                        }

                        Game::remove_first(&mut new_hand, district.name)
                            .ok_or("card not in hand")?;

                        let active = self.active_player_mut().unwrap();
                        active.gold -= cost;
                        active.hand = new_hand;

                        let target = self.players[target.0].borrow_mut();
                        target.gold -= discard.len();
                        target.hand.append(&mut copy);
                    }

                    BuildMethod::ThievesDen { discard } => {
                        if district.name != DistrictName::ThievesDen {
                            Err("You are not building the ThievesDen!")?;
                        }
                        if discard.len() > cost {
                            Err("Cannot discard more cards than the cost")?;
                        }

                        if active.gold + discard.len() < cost {
                            Err("Not enough gold or cards discarded")?;
                        }

                        cost -= discard.len();
                        let mut discard_set = discard.clone();
                        let mut new_hand = Vec::with_capacity(active.hand.len());
                        for card in active.hand.iter() {
                            if let Some((i, _)) =
                                discard_set.iter().enumerate().find(|(_, d)| *d == card)
                            {
                                discard_set.swap_remove(i);
                            } else {
                                new_hand.push(*card);
                            }
                        }

                        if discard_set.len() > 0 {
                            Err("Can't discard cards not in your hand")?;
                        }
                        Game::remove_first(&mut new_hand, district.name)
                            .ok_or("card not in hand")?;

                        let active = self.active_player_mut().unwrap();
                        active.gold -= cost;
                        active.hand = new_hand;
                        for card in discard {
                            self.deck.discard_to_bottom(*card);
                        }
                    }

                    BuildMethod::Framework { .. } => {
                        let city_index = active
                            .city
                            .iter()
                            .enumerate()
                            .find_map(|(i, c)| {
                                if c.name == DistrictName::Framework {
                                    Some(i)
                                } else {
                                    None
                                }
                            })
                            .ok_or("You don't own a framework!")?;

                        let active = self.active_player_mut().unwrap();
                        Game::remove_first(&mut active.hand, district.name)
                            .ok_or("card not in hand")?;
                        active.city.swap_remove(city_index);
                    }

                    BuildMethod::Necropolis { sacrifice: target } => {
                        if district.name != DistrictName::Necropolis {
                            Err("You are not building the necropolis!")?;
                        }
                        let city_index = active
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
                            .ok_or("Cannot sacrifice a district you don't own!")?;

                        let active = self.active_player_mut().unwrap();
                        Game::remove_first(&mut active.hand, district.name)
                            .ok_or("card not in hand")?;

                        let district = active.city.swap_remove(city_index);
                        self.discard_district(district.name);
                    }
                }

                if !is_free_build {
                    self.remaining_builds -= 1;
                }

                if self.active_role().unwrap().role != RoleName::TaxCollector
                    && self.characters.has_tax_collector()
                {
                    let player = self.active_player_mut()?;
                    if player.gold > 0 {
                        player.gold -= 1;
                        self.tax_collector += 1;
                    }
                }

                // the magistrate can only confiscate the first build of a turn
                if self.active_role().unwrap().has_warrant()
                    && !self.turn_actions.iter().any(|act| act.is_build())
                {
                    ActionOutput::new(format!(
                        "{} begins to build a {}; waiting on the Magistrate's response.",
                        self.active_player().unwrap().name,
                        district.display_name
                    ))
                    .followup(Followup::Warrant {
                        magistrate: self.characters.get(Rank::One).player.unwrap(),
                        gold: cost,
                        district: district.name,
                        signed: self
                            .active_role()
                            .unwrap()
                            .markers
                            .iter()
                            .any(|m| *m == Marker::Warrant { signed: true }),
                    })
                } else {
                    self.complete_build(self.active_player().unwrap().index, cost, district.name);
                    ActionOutput::new(format!(
                        "{} build a {}.",
                        self.active_player().unwrap().name,
                        district.display_name
                    ))
                }
            }

            Action::TakeCrown => {
                // Hard coded to rank four. This overrides the Witch.
                self.crowned = self
                    .characters
                    .get(Rank::Four)
                    .player
                    .ok_or("No Royalty to take crown!")?;

                ActionOutput::new(format!(
                    "{} takes the crown.",
                    self.players[self.crowned.0].name,
                ))
            }

            Action::Assassinate { role } => {
                if role.rank() == Rank::One {
                    return Err("cannot kill self".into());
                }
                let target = self.characters.get_mut(role.rank());
                target.markers.push(Marker::Killed);

                ActionOutput::new(format!(
                    "The Assassin ({}) kills the {}; Their turn will be skipped.",
                    self.active_player()?.name,
                    role.display_name(),
                ))
            }

            Action::Steal { role } => {
                if role.rank() < Rank::Three {
                    return Err("target rank is too low".into());
                }

                let target = self.characters.get_mut(role.rank());

                if target
                    .markers
                    .iter()
                    .any(|marker| *marker == Marker::Killed)
                {
                    return Err("cannot rob from the dead".into());
                }

                if target
                    .markers
                    .iter()
                    .any(|marker| *marker == Marker::Bewitched)
                {
                    return Err("cannot rob from the bewitched".into());
                }

                target.markers.push(Marker::Robbed);

                ActionOutput::new(format!( 
                    "The Thief ({}) robs the {}; At the start of their turn, all their gold will be taken.",
                    self.active_player()?.name,
                    role.display_name(),
                ))
            }

            Action::Magic(MagicianAction::TargetPlayer { player }) => {
                let mut hand = std::mem::take(&mut self.active_player_mut()?.hand);
                let target = if let Some(p) = self.players.iter_mut().find(|p| p.name == *player) {
                    p
                } else {
                    // put the hand back;
                    self.active_player_mut()?.hand = hand;
                    return Err("invalid target".into());
                };

                let hand_count = hand.len();
                let target_count = target.hand.len();

                std::mem::swap(&mut hand, &mut target.hand);
                self.active_player_mut()?.hand = hand;

                ActionOutput::new(format!(
                    "The Magician ({}) swaps their hand of {} cards with {}'s hand of {} cards.",
                    self.active_player()?.name,
                    hand_count,
                    player,
                    target_count,
                ))
            }

            Action::Magic(MagicianAction::TargetDeck { district }) => {
                let active = self.active_player_mut()?;
                let mut discard = district.clone();
                let mut new_hand = Vec::with_capacity(active.hand.len());

                for card in active.hand.iter() {
                    if let Some((i, _)) = discard.iter().enumerate().find(|(_, d)| *d == card) {
                        discard.swap_remove(i);
                    } else {
                        new_hand.push(*card);
                    }
                }

                if discard.len() > 0 {
                    Err("Can't discard cards not in your hand")?;
                }
                active.hand = new_hand;

                for card in discard.iter() {
                    self.deck.discard_to_bottom(*card);
                }

                self.gain_cards(discard.len());

                ActionOutput::new(format!(
                    "The Magician ({}) discarded {} cards and drew {} more.",
                    self.active_player()?.name,
                    discard.len(),
                    discard.len(),
                ))
            }

            Action::WarlordDestroy { district: target } => {
                if target.district == DistrictName::Keep {
                    return Err("Cannot target the Keep".into());
                }

                let available_gold = self.active_player()?.gold;
                let complete_size = self.complete_city_size();
                let player = self
                    .players
                    .iter_mut()
                    .find(|p| p.name == target.player)
                    .ok_or("Player does not exist")?;

                if self.characters.has_bishop_protection(player.index) {
                    Err("Cannot target the Bishop")?
                }
                if player.city_size() >= complete_size {
                    Err("Cannot target a completed city")?
                }

                let city_index = player
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

                let mut destroy_cost = target.effective_cost() - 1;
                if player.city_has(DistrictName::GreatWall) {
                    destroy_cost += 1;
                }

                if available_gold < destroy_cost {
                    return Err("not enough gold to destroy".into());
                }

                player.city.remove(city_index);
                self.active_player_mut()?.gold -= destroy_cost;
                self.discard_district(target.district);

                ActionOutput::new(format!(
                    "The Warlord ({}) destroys {}'s {}.",
                    self.active_player()?.name,
                    target.player,
                    target.district.data().display_name,
                ))
            }

            Action::Armory { district: target } => {
                if target.district == DistrictName::Keep {
                    return Err("Cannot destroy the Keep".into());
                }

                if target.district == DistrictName::Armory {
                    return Err("The armory cannot destroy itself".into());
                }

                let complete_size = self.complete_city_size();
                let targeted_player = self
                    .players
                    .iter_mut()
                    .find(|p| p.name == target.player && p.city_size() < complete_size)
                    .ok_or("player does not exist or cannot destroy from complete city")?;

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

                targeted_player.city.remove(city_index);
                let active_player = self.active_player_mut()?;
                let (city_index, _) = active_player
                    .city
                    .iter()
                    .enumerate()
                    .find(|(_, d)| d.name == DistrictName::Armory)
                    .ok_or("You do not have the armory")?;

                active_player.city.remove(city_index);
                self.discard_district(DistrictName::Armory);
                self.discard_district(target.district);

                ActionOutput::new(format!(
                    "{} sacrifices their Armory to destroy {}'s {}.",
                    self.active_player()?.name,
                    target.player,
                    target.district.data().display_name,
                ))
            }

            Action::Beautify {
                district: CityDistrictTarget { district, .. },
            } => {
                let player = self.active_player_mut()?;

                if player.gold < 1 {
                    return Err("Not enough gold".into());
                }

                let city_district = player
                    .city
                    .iter_mut()
                    .find(|d| !d.beautified && d.name == *district)
                    .ok_or("Invalid target. Is it already beautified?")?;

                city_district.beautified = true;
                player.gold -= 1;

                ActionOutput::new(format!(
                    "The Artist ({}) beautifies their {}.",
                    self.active_player()?.name,
                    district.data().display_name,
                ))
            }

            Action::NavigatorGain {
                resource: Resource::Cards,
            } => {
                self.gain_cards(4);
                ActionOutput::new(format!(
                    "The Navigator ({}) gains 4 extra cards.",
                    self.active_player()?.name
                ))
            }

            Action::NavigatorGain {
                resource: Resource::Gold,
            } => {
                let player = self.active_player_mut()?;
                player.gold += 4;

                ActionOutput::new(format!(
                    "The Navigator ({}) gains 4 extra gold.",
                    player.name
                ))
            }

            Action::SeerTake => {
                let my_index = self.active_player_index()?;
                let mut taken_from = Vec::with_capacity(self.players.len() - 1);
                let mut active_hand = mem::replace(&mut self.active_player_mut()?.hand, Vec::new());
                for player in self.players.iter_mut() {
                    if player.index != my_index && player.hand.len() > 0 {
                        let i = self.rng.gen_range(0..player.hand.len());
                        let card = player.hand.remove(i);
                        taken_from.push(player.index);
                        active_hand.push(card);
                    }
                }
                self.active_player_mut()?.hand = active_hand;
                ActionOutput::new(format!(
                    "The Seer ({}) takes 1 card from everyone.",
                    self.active_player()?.name
                ))
                .maybe_followup(if taken_from.is_empty() {
                    None
                } else {
                    Some(Followup::SeerDistribute {
                        players: taken_from,
                    })
                })
            }

            Action::SeerDistribute { seer } => {
                let players: HashMap<_, _> = self
                    .players
                    .iter()
                    .map(|p| (p.name.borrow(), p.index))
                    .collect();
                let pairs: Vec<(PlayerIndex, DistrictName)> = seer
                    .iter()
                    .map(|(name, district)| {
                        Ok((
                            *players
                                .get(name)
                                .ok_or(format!("Cannot give {} a card.", name).to_owned())?,
                            *district,
                        ))
                    })
                    .collect::<Result<_>>()?;

                let mut removed = Vec::new();
                for (_, district) in pairs.iter() {
                    let active_hand = &mut self.active_player_mut()?.hand;
                    if let Some(district) = Game::remove_first(active_hand, *district) {
                        removed.push(district);
                    } else {
                        return Err("cannot assign district not in hand!".into());
                    }
                }
                for (index, district) in pairs {
                    self.players[index.0].hand.push(district);
                }

                ActionOutput::new(format!("The Seer gives cards back."))
            }

            Action::ResourcesFromReligion { gold, cards, .. } => {
                let player = self.active_player()?;
                let count = player.count_suit_for_resource_gain(CardSuit::Religious);
                if gold + cards < count {
                    return Err(format!("Too few resources, you should select {}", count).into());
                }

                if gold + cards > count {
                    return Err(format!("Too many resources, you should select {}", count).into());
                }

                let _amount = self.gain_cards(count);
                let player = self.active_player()?;

                ActionOutput::new(format!(
                    "The Abbot ({}) gained {} gold and {} cards from their Religious districts",
                    player.name, gold, cards
                ))
            }
            Action::CollectTaxes => {
                let taxes = self.tax_collector;
                self.active_player_mut()?.gold += taxes;
                self.tax_collector = 0;
                ActionOutput::new(format!(
                    "The Tax Collector collects {} gold in taxes.",
                    taxes
                ))
            }
            Action::QueenGainGold => {
                let active = self.active_player_index()?;
                let left = PlayerIndex((active.0 + self.players.len() - 1) % self.players.len());
                let right = PlayerIndex((active.0 + 1) % self.players.len());
                let c = self.characters.get(Rank::Four);
                let log = if c.revealed && c.player.is_some_and(|p| p == left || p == right) {
                    self.players[active.0].gold += 3;
                    format!(
                        "The Queen is seated next to the {}, and gains 3 gold.",
                        c.role.display_name()
                    )
                } else {
                    format!(
                        "The Queen is not seated next to the {}.",
                        c.role.display_name()
                    )
                };
                ActionOutput::new(log)
            }

            Action::SpyAcknowledge => {
                //
                ActionOutput::new(format!("Spy is done peeking at the revealed hand"))
            }

            Action::Spy { player, suit } => {
                if player == self.active_player().unwrap().name {
                    return Err("Cannot spy on self.".into());
                }
                let target = self
                    .players
                    .iter()
                    .find_map(|p| {
                        if &p.name == player {
                            Some(p.index)
                        } else {
                            None
                        }
                    })
                    .ok_or("no player with that name")?;
                let matches = self.players[target.0]
                    .hand
                    .iter()
                    .filter(|d| d.data().suit == *suit)
                    .count();
                let gold_taken = std::cmp::min(self.players[target.0].gold, matches);
                self.players[target.0].gold -= gold_taken;
                self.active_player_mut().unwrap().gold += gold_taken;
                let cards_drawn = self.gain_cards(matches);
                ActionOutput::new(format!(
                    "The Spy ({}) is counting {} districts. They spy on {}, and find {} matches. They take {} gold, and draw {} cards.",
                    self.active_player()?.name,
                    suit,
                    self.players[target.0].name,
                    matches,
                    gold_taken,
                    cards_drawn
                )).followup(
                    Followup::SpyAcknowledge {
                        player: self.players[target.0].name.clone(),
                        revealed: self.players[target.0].hand.clone(),
                    },
                )
            }
            Action::TakeFromRich { player } => {
                if player == self.active_player().unwrap().name {
                    return Err("Cannot take from yourself".into());
                }

                let my_gold = self.active_player().unwrap().gold;

                let mut richest = Vec::with_capacity(self.players.len());
                for player in self.players.iter_mut().filter(|p| p.gold > my_gold) {
                    if richest.len() == 0 {
                        richest.push(player);
                    } else if player.gold == richest[0].gold {
                        richest.push(player);
                    } else if player.gold > richest[0].gold {
                        richest.clear();
                        richest.push(player);
                    }
                }

                let target = richest
                    .iter_mut()
                    .find(|p| p.name == *player)
                    .ok_or("Not among the richest".to_owned())?;

                target.gold -= 1;
                let name = target.name.clone();
                self.active_player_mut().unwrap().gold += 1;
                ActionOutput::new(format!(
                    "The Abbot ({}) takes 1 gold from the richest: {}",
                    self.active_player()?.name,
                    name
                ))
            }
            Action::SendWarrants { signed, unsigned } => {
                let mut roles = Vec::with_capacity(3);
                roles.push(signed);
                for role in unsigned {
                    if roles.iter().any(|r| *r == role) {
                        return Err("Cannot assign more than 1 warrant to a role".into());
                    }
                    roles.push(role);
                }
                if roles.iter().any(|role| role.rank() == Rank::One) {
                    return Err("Cannot assign warrant to self".into());
                }
                roles.sort_by_key(|r| r.rank());

                self.characters
                    .get_mut(signed.rank())
                    .markers
                    .push(Marker::Warrant { signed: true });

                for role in unsigned {
                    self.characters
                        .get_mut(role.rank())
                        .markers
                        .push(Marker::Warrant { signed: false });
                }
                ActionOutput::new(format!(
                    "The Magistrate ({}) sends warrants to the {}, the {}, and the {}.",
                    self.active_player().unwrap().name,
                    roles[0].display_name(),
                    roles[1].display_name(),
                    roles[2].display_name()
                ))
            }
            Action::Blackmail { flowered, unmarked } => {
                if flowered == unmarked {
                    return Err("Cannot blackmail someone twice. ".into());
                }
                if flowered.rank() < Rank::Three || unmarked.rank() < Rank::Three {
                    return Err("Can only blackmail rank 3 or higher".into());
                }

                if self
                    .characters
                    .get(flowered.rank())
                    .markers
                    .iter()
                    .any(|m| *m == Marker::Killed || *m == Marker::Bewitched)
                {
                    return Err("Cannot blackmail the killed or bewitched".into());
                }
                if self
                    .characters
                    .get(unmarked.rank())
                    .markers
                    .iter()
                    .any(|m| *m == Marker::Killed || *m == Marker::Bewitched)
                {
                    return Err("Cannot blackmail the killed or bewitched".into());
                }

                self.characters
                    .get_mut(flowered.rank())
                    .markers
                    .push(Marker::Blackmail { flowered: true });

                self.characters
                    .get_mut(unmarked.rank())
                    .markers
                    .push(Marker::Blackmail { flowered: false });
                let mut roles = vec![flowered, unmarked];
                roles.sort_by_key(|r| r.rank());

                ActionOutput::new(format!(
                    "The Blackmailer ({}) sends blackmail to the {} and the {}",
                    self.active_player().unwrap().name,
                    roles[0].display_name(),
                    roles[1].display_name(),
                ))
            }

            Action::Smithy => {
                let active = self.active_player_mut()?;
                if active.gold < 2 {
                    Err("Not enough gold")?;
                }
                active.gold -= 2;
                self.gain_cards(3);
                ActionOutput::new(format!(
                    "At the Smithy, {} forges 2 gold into 3 cards.",
                    self.active_player()?.name
                ))
            }

            Action::Laboratory { district } => {
                let active = self.active_player_mut()?;
                let (index, _) = active
                    .hand
                    .iter()
                    .enumerate()
                    .find(|(_, name)| *name == district)
                    .ok_or("district not in hand")?;
                let card = active.hand.remove(index);
                active.gold += 2;
                self.deck.discard_to_bottom(card);

                ActionOutput::new(format!(
                    "At the Laboratory, {} transmutes 1 card into 2 gold.",
                    self.active_player()?.name
                ))
            }

            Action::Museum { district } => {
                let active = self.active_player_mut()?;
                let (index, _) = active
                    .hand
                    .iter()
                    .enumerate()
                    .find(|(_, name)| *name == district)
                    .ok_or("district not in hand")?;
                let card = active.hand.remove(index);
                self.museum.tuck(card);

                ActionOutput::new(format!(
                    "{} tucks a card face down under their Museum.",
                    self.active_player()?.name
                ))
            }

            Action::ScholarReveal => {
                let drawn = self.deck.draw_many(7).collect::<Vec<_>>();

                ActionOutput::new(format!(
                    "The Scholar ({}) is choosing from the top {} cards of the deck.",
                    self.active_player()?.name,
                    drawn.len(),
                ))
                .followup(Followup::ScholarPick { revealed: drawn })
            }

            Action::ScholarPick { district } => {
                let mut revealed =
                    if let Some(Followup::ScholarPick { revealed, .. }) = self.followup.as_mut() {
                        revealed
                    } else {
                        return Err("action is not allowed".into());
                    };

                Game::remove_first(&mut revealed, *district).ok_or("invalid choice")?;
                for remaining in revealed {
                    self.deck.discard_to_bottom(*remaining);
                }
                self.deck.shuffle(&mut self.rng);
                self.active_player_mut()?.hand.push(*district);

                ActionOutput::new(format!(
                    "The Scholar ({}) picks a card, discarding the rest and shuffling the deck.",
                    self.active_player()?.name,
                ))
            }

            Action::TheaterPass => {
                //
                ActionOutput::new(format!(
                    "{} decided not to use the Theatre",
                    self.active_player()?.name
                ))
                .end_turn()
            }
            Action::Theater { role, player } => {
                if self.active_player().unwrap().name == *player {
                    return Err("Cannot swap with self".into());
                }
                Game::remove_first(&mut self.active_player_mut()?.roles, *role)
                    .ok_or("You cannot give away a role you don't have")?;

                let target = self
                    .players
                    .iter_mut()
                    .find(|p| p.name == *player)
                    .ok_or("nonexistent player")?;

                let index = self.rng.gen_range(0..target.roles.len());
                let target_role = target.roles.swap_remove(index);
                target.roles.push(*role);
                target.roles.sort_by_key(|r| r.rank());
                for role in target.roles.iter() {
                    self.characters.get_mut(role.rank()).player = Some(target.index);
                }

                let active = self.active_player_mut().unwrap();
                active.roles.push(target_role);
                active.roles.sort_by_key(|r| r.rank());
                let index = active.index;
                for role in active.roles.clone() {
                    self.characters.get_mut(role.rank()).player = Some(index);
                }

                ActionOutput::new(format!(
                    "Theater: {} swaps roles with {}",
                    self.active_player().unwrap().name,
                    player
                ))
                .end_turn()
            }

            Action::MarshalSeize { district: target } => {
                if target.district == DistrictName::Keep {
                    return Err("Cannot target the Keep".into());
                }
                if self.active_player().unwrap().city_has(target.district) {
                    return Err("Cannot seize a copy of your own district".into());
                }
                let available_gold = self.active_player()?.gold;
                let complete_size = self.complete_city_size();
                let player = self
                    .players
                    .iter_mut()
                    .find(|p| p.name == target.player)
                    .ok_or("Player does not exist")?;

                if self.characters.has_bishop_protection(player.index) {
                    Err("Cannot target the Bishop")?
                }
                if player.city_size() >= complete_size {
                    Err("Cannot target a completed city")?
                }

                let city_index = player
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
                let mut seize_cost = target.effective_cost();
                if seize_cost > 3 {
                    return Err("Cannot seize district because it costs more than 3".into());
                }
                if player.city_has(DistrictName::GreatWall) {
                    seize_cost += 1;
                }

                if available_gold < seize_cost {
                    return Err("Not enough gold to seize".into());
                }

                let district = player.city.remove(city_index);
                player.gold += seize_cost;
                let active = self.active_player_mut().unwrap();
                active.gold -= seize_cost;
                active.city.push(district);

                ActionOutput::new(format!(
                    "The Marshal ({}) seizes {}'s {}.",
                    self.active_player()?.name,
                    target.player,
                    target.district.data().display_name,
                ))
            }

            Action::EmperorGiveCrown { player, resource } => {
                if self.active_player().unwrap().name == *player {
                    Err("Cannot give the crown to yourself")?;
                }

                let target = self
                    .players
                    .iter_mut()
                    .find(|p| p.name == *player)
                    .ok_or("Player does not exist")?;

                if target.index == self.crowned {
                    Err("Cannot give the crown to the already crowned player")?;
                }

                self.crowned = target.index;

                match resource {
                    Resource::Gold if target.gold > 0 => {
                        target.gold -= 1;
                        self.active_player_mut().unwrap().gold += 1;
                    }
                    Resource::Cards if target.hand.len() > 0 => {
                        let index = self.rng.gen_range(0..target.hand.len());
                        let card = target.hand.remove(index);
                        self.active_player_mut().unwrap().hand.push(card);
                    }
                    _ => {}
                }

                ActionOutput::new(format!(
                    "The Emperor ({}) gives {} the crown and takes one of their {}.",
                    self.active_player()?.name,
                    player,
                    match resource {
                        Resource::Gold => "gold",
                        Resource::Cards => "cards",
                    }
                ))
            }

            Action::EmperorHeirGiveCrown { player } => {
                if self.active_player().unwrap().name == *player {
                    Err("Cannot give the crown to yourself")?;
                }

                let target = self
                    .players
                    .iter_mut()
                    .find(|p| p.name == *player)
                    .ok_or("Player does not exist")?;

                if target.index == self.crowned {
                    Err("Cannot give the crown to the already crowned player")?;
                }

                self.crowned = target.index;

                ActionOutput::new(format!(
                    "The Emperor's advisor ({}) gives {} the crown.",
                    self.active_player()?.name,
                    player,
                ))
                .end_turn()
            }
            Action::DiplomatTrade {
                district: my_target,
                theirs: their_target,
            } => {
                if their_target.district == DistrictName::Keep {
                    return Err("Cannot target the Keep".into());
                }

                let complete_city_size = self.complete_city_size();
                let player = self
                    .players
                    .iter()
                    .find(|p| p.name == their_target.player)
                    .ok_or("invalid player target")?;

                if self.characters.has_bishop_protection(player.index) {
                    Err("Cannot target the Bishop")?
                }
                if player.city_size() >= complete_city_size {
                    Err("Cannot target a completed city")?
                }

                let my_cost = my_target.effective_cost();
                let their_cost = their_target.effective_cost();
                let mut trade_cost = if my_cost < their_cost {
                    their_cost - my_cost
                } else {
                    0
                };

                if player.city_has(DistrictName::GreatWall) {
                    trade_cost += 1;
                }
                if trade_cost > self.active_player().unwrap().gold {
                    Err("Not enough gold")?
                }

                if player.city_has(my_target.district) {
                    Err("The targeted player already has a copy of that district")?
                }

                if self
                    .active_player()
                    .unwrap()
                    .city_has(their_target.district)
                {
                    Err("You already have a copy of that district")?
                }

                let my_city_index = self
                    .active_player()
                    .unwrap()
                    .city
                    .iter()
                    .enumerate()
                    .find_map(|(i, c)| {
                        if c.name == my_target.district && c.beautified == my_target.beautified {
                            Some(i)
                        } else {
                            None
                        }
                    })
                    .ok_or("does not exist in the your city")?;

                let their_city_index = player
                    .city
                    .iter()
                    .enumerate()
                    .find_map(|(i, c)| {
                        if c.name == their_target.district
                            && c.beautified == their_target.beautified
                        {
                            Some(i)
                        } else {
                            None
                        }
                    })
                    .ok_or("does not exist in the targeted player's city")?;
                let index = player.index.0;
                let player = self.players[index].borrow_mut();
                player.gold += trade_cost;
                player.city[their_city_index] = CityDistrict {
                    name: my_target.district,
                    beautified: my_target.beautified,
                };

                let active = self.active_player_mut().unwrap();
                active.gold -= trade_cost;
                active.city[my_city_index] = CityDistrict {
                    name: their_target.district,
                    beautified: their_target.beautified,
                };

                ActionOutput::new(format!(
                    "The Diplomat ({}) traded their {} for {}'s {}{}.",
                    active.name,
                    my_target.district.data().display_name,
                    their_target.player,
                    their_target.district.data().display_name,
                    if trade_cost > 0 {
                        format!("; they paid {} gold for the difference", trade_cost)
                    } else {
                        "".into()
                    }
                ))
            }
            Action::WizardPeek { player } => {
                let target = self
                    .players
                    .iter()
                    .find(|p| p.name == *player)
                    .ok_or("invalid player target")?;

                ActionOutput::new(format!(
                    "The Wizard ({}) peeks at {}'s hand.",
                    self.active_player().unwrap().name,
                    player,
                ))
                .followup(Followup::WizardPick {
                    player: target.index,
                })
            }
            Action::WizardPick(WizardMethod::Pick { district }) => match self.followup {
                Some(Followup::WizardPick { player: target }) => {
                    Game::remove_first(&mut self.players[target.0].hand, *district)
                        .ok_or("district not in target player's hand")?;
                    self.active_player_mut().unwrap().hand.push(*district);
                    ActionOutput::new(format!(
                        "The Wizard ({}) takes a card from {}'s hand.",
                        self.active_player().unwrap().name,
                        self.players[target.0].name,
                    ))
                }
                _ => Err("impossible")?,
            },

            Action::WizardPick(method) => match self.followup {
                Some(Followup::WizardPick { player: target }) => {
                    let district = match method {
                        WizardMethod::Pick { .. } => Err("Impossible!")?,
                        WizardMethod::Build { district } => *district,
                        WizardMethod::Framework { district } => *district,
                        WizardMethod::ThievesDen { .. } => DistrictName::ThievesDen,
                        WizardMethod::Necropolis { .. } => DistrictName::Necropolis,
                    };

                    if self.players[target.0].hand.iter().all(|d| *d != district) {
                        Err("Card not in hand")?;
                    }

                    let active = self.active_player().unwrap();
                    if district == DistrictName::Monument && active.city.len() >= 5 {
                        return Err("You can only build the Monument, if you have less than 5 districts in your city".into());
                    }

                    let district = district.data();

                    let mut cost = district.cost;
                    if district.suit == CardSuit::Unique
                        && active.city.iter().any(|d| d.name == DistrictName::Factory)
                    {
                        cost -= 1;
                    }

                    match method {
                        WizardMethod::Pick { .. } => Err("Impossible")?,
                        WizardMethod::Build { .. } => {
                            if cost > active.gold {
                                Err("Not enough gold")?;
                            }

                            let active = self.active_player_mut()?;
                            active.gold -= cost;
                        }
                        WizardMethod::ThievesDen { discard } => {
                            if district.name != DistrictName::ThievesDen {
                                Err("You are not building the ThievesDen!")?;
                            }
                            if discard.len() > cost {
                                Err("Cannot discard more cards than the cost")?;
                            }

                            if active.gold + discard.len() < cost {
                                Err("Not enough gold or cards discarded")?;
                            }

                            cost -= discard.len();
                            let mut discard_set = discard.clone();
                            let mut new_hand = Vec::with_capacity(active.hand.len());
                            for card in active.hand.iter() {
                                if let Some((i, _)) =
                                    discard_set.iter().enumerate().find(|(_, d)| *d == card)
                                {
                                    discard_set.swap_remove(i);
                                } else {
                                    new_hand.push(*card);
                                }
                            }

                            if discard_set.len() > 0 {
                                Err("Can't discard cards not in your hand")?;
                            }

                            let active = self.active_player_mut().unwrap();
                            active.gold -= cost;
                            active.hand = new_hand;
                            for card in discard {
                                self.deck.discard_to_bottom(*card);
                            }
                        }

                        WizardMethod::Framework { .. } => {
                            let city_index = active
                                .city
                                .iter()
                                .enumerate()
                                .find_map(|(i, c)| {
                                    if c.name == DistrictName::Framework {
                                        Some(i)
                                    } else {
                                        None
                                    }
                                })
                                .ok_or("Cannot sacrifice a district you don't own!")?;

                            let active = self.active_player_mut().unwrap();
                            active.city.swap_remove(city_index);
                        }

                        WizardMethod::Necropolis { sacrifice: target } => {
                            if district.name != DistrictName::Necropolis {
                                Err("You are not building the necropolis!")?;
                            }
                            let city_index = active
                                .city
                                .iter()
                                .enumerate()
                                .find_map(|(i, c)| {
                                    if c.name == target.district
                                        && c.beautified == target.beautified
                                    {
                                        Some(i)
                                    } else {
                                        None
                                    }
                                })
                                .ok_or("Cannot sacrifice a district you don't own!")?;

                            let active = self.active_player_mut().unwrap();
                            let district = active.city.swap_remove(city_index);
                            self.discard_district(district.name);
                        }
                    }
                    Game::remove_first(&mut self.players[target.0].hand, district.name).unwrap();

                    if self.characters.has_tax_collector() {
                        let player = self.active_player_mut()?;
                        if player.gold > 0 {
                            player.gold -= 1;
                            self.tax_collector += 1;
                        }
                    }

                    // the magistrate can only confiscate the first build of a turn
                    if self.active_role().unwrap().has_warrant()
                        && !self.turn_actions.iter().any(|act| act.is_build())
                    {
                        ActionOutput::new(
                             format!(
                                "The Wizard begins to build a {}; waiting on the Magistrate's response.",
                                district.display_name
                           ) ).followup(
                            Followup::Warrant {
                                magistrate: self.characters.get(Rank::One).player.unwrap(),
                                gold: cost,
                                district: district.name,
                                signed: self
                                    .active_role()
                                    .unwrap()
                                    .markers
                                    .iter()
                                    .any(|m| *m == Marker::Warrant { signed: true }),
                            })
                    } else {
                        self.complete_build(
                            self.active_player().unwrap().index,
                            cost,
                            district.name,
                        );

                        ActionOutput::new(format!("The Wizard builds a {}.", district.display_name))
                    }
                }

                _ => Err("impossible")?,
            },
            Action::Bewitch { role } => {
                if role.rank() == Rank::One {
                    Err("Cannot target self")?;
                }
                self.characters
                    .get_mut(role.rank())
                    .markers
                    .push(Marker::Bewitched);

                ActionOutput::new(format!("The Witch bewitches {}.", role.display_name()))
                    .end_turn()
            }
        })
    }

    pub fn abbot_take_from_rich_targets(&self) -> Vec<&Player> {
        let my_gold = self.active_player().unwrap().gold;

        let mut richest = Vec::with_capacity(self.players.len());
        for player in self.players.iter().filter(|p| p.gold > my_gold) {
            if richest.len() == 0 {
                richest.push(player);
            } else if player.gold == richest[0].gold {
                richest.push(player);
            } else if player.gold > richest[0].gold {
                richest.clear();
                richest.push(player);
            }
        }
        richest
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
        let amount = player.count_suit_for_resource_gain(suit);
        player.gold += amount;

        Ok(ActionOutput::new(format!(
            "The {} ({}) gains {} gold from their {} districts.",
            self.active_role()?.role.display_name(),
            self.active_player()?.name,
            amount,
            suit
        )))
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

        Ok(ActionOutput::new(format!(
            "The {} ({}) gains {} cards from their {} districts.",
            self.active_role()?.role.display_name(),
            self.active_player()?.name,
            amount,
            suit
        )))
    }

    fn end_turn(&mut self) -> Result<()> {
        log::info!("ending turn");
        self.turn_actions.clear();

        match self.active_turn.borrow_mut() {
            Turn::GameOver => {}
            Turn::Draft(Draft {
                theater_step: true, ..
            }) => {
                // call
                self.active_turn = Turn::Call(Call {
                    rank: Rank::One,
                    end_of_round: false,
                })
            }
            Turn::Draft(draft) => {
                // discard cards between turns

                // for the 3 player game with 9 characters
                // after the first round of cards are selected,
                // randomly discard 1.
                if self.players.len() == 3
                    && self.characters.len() == 9
                    && draft.remaining.len() == 5
                {
                    let index = self.rng.gen_range(0..draft.remaining.len());
                    draft.remaining.remove(index);
                }

                // for the 7 player 8 role game, or 8 player 9 role game
                // give the final player the choice to choose the initial discard
                if self.players.len() + 1 == self.characters.len()
                    && draft.remaining.len() == 1
                    && draft.initial_discard.is_some()
                {
                    let initial = draft.initial_discard.take().ok_or("impossible")?;
                    draft.remaining.push(initial);
                }

                // advance turn
                let role_count = if self.players.len() <= 3 { 2 } else { 1 };
                if self.players.iter().all(|p| p.roles.len() == role_count) {
                    if let Some(player) = self
                        .players
                        .iter()
                        .find(|p| p.city_has(DistrictName::Theater))
                    {
                        draft.player = player.index;
                        draft.theater_step = true;
                    } else {
                        self.active_turn = Turn::Call(Call {
                            rank: Rank::One,
                            end_of_round: false,
                        });
                    }
                } else {
                    draft.player = PlayerIndex((draft.player.0 + 1) % self.players.len());
                };
            }
            Turn::Call(Call {
                end_of_round: true, ..
            }) => {
                self.end_round();
            }
            Turn::Call(_) => {
                if let Ok(player) = self.active_player() {
                    let role = self.active_role().unwrap().role;
                    let poor_house = role != RoleName::Witch
                        && player.gold == 0
                        && player.city_has(DistrictName::PoorHouse);

                    let park = role != RoleName::Witch
                        && player.hand.len() == 0
                        && player.city_has(DistrictName::Park);
                    let name = self.active_player().unwrap().name.clone();
                    if poor_house {
                        self.active_player_mut().unwrap().gold += 1;
                        self.active_role_mut()
                            .unwrap()
                            .logs
                            .push(format!("{} gains 1 gold from their Poor House.", name).into());
                    }

                    if park {
                        self.gain_cards(2);
                        self.active_role_mut()
                            .unwrap()
                            .logs
                            .push(format!("{} gains 2 cards from their Park.", name).into());
                    }

                    let refund = self.alchemist;
                    if refund > 0 {
                        self.alchemist = 0;
                        self.active_player_mut().unwrap().gold += refund;
                        self.active_role_mut().unwrap().logs.push(
                            format!("The Alchemist is refunded {} gold spent building.", refund)
                                .into(),
                        );
                    }
                }
                self.call_next();
            }
        }
        self.start_turn()?;
        Ok(())
    }

    fn call_next(&mut self) {
        match self.active_turn.borrow() {
            Turn::Call(call) => {
                let o = self.characters.next(call.rank);
                if let Some(rank) = o {
                    self.active_turn = Turn::Call(Call {
                        rank,
                        end_of_round: false,
                    });
                } else if self.characters.get(Rank::Four).role == RoleName::Emperor {
                    self.active_turn = Turn::Call(Call {
                        rank: Rank::Four,
                        end_of_round: true,
                    });
                } else {
                    self.end_round();
                };
            }
            _ => {}
        }
    }

    pub fn end_round(&mut self) {
        // triggered actions
        let rank = Rank::Four;
        let character = &self.characters.get(rank);
        if character.markers.iter().any(|m| *m == Marker::Killed) {
            if let Some(index) = character.player {
                if character.role == RoleName::King || character.role == RoleName::Patrician {
                    self.crowned = index;
                    self.logs.push(
                        format!(
                            "{}'s heir {} crowned.",
                            character.role.display_name(),
                            self.players[index.0].name
                        )
                        .into(),
                    );
                }

                if self.characters.len() >= 9 {
                    let n = self.players.len();
                    let ninth = self.characters.get(Rank::Nine);
                    let p2 = index;
                    if ninth.role == RoleName::Queen
                        && ninth
                            .player
                            .is_some_and(|p1| ((p1.0 + 1) % n == p2.0 || (p2.0 + 1) % n == p1.0))
                    {
                        self.players[ninth.player.unwrap().0].gold += 3;
                        self.logs.push(
                            format!(
                                "The Queen ({}) is seated next to the dead {}; they gain 3 gold.",
                                self.players[ninth.player.unwrap().0].name,
                                character.role.display_name()
                            )
                            .into(),
                        );
                    }
                }
            }
        }

        // GAME OVER
        if self.first_to_complete.is_some() {
            self.active_turn = Turn::GameOver;
            return;
        }

        self.cleanup_round();

        self.begin_draft();
    }

    fn cleanup_round(&mut self) {
        for character in self.characters.iter_mut() {
            character.cleanup_round();
        }
        for player in self.players.iter_mut() {
            player.cleanup_round();
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
