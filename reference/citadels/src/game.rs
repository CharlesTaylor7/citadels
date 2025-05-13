use crate::actions::{Action, ActionTag};
use crate::districts::DistrictName;
use crate::game_actions::perform_action;
use crate::lobby::{self, Lobby};
use crate::museum::Museum;
use crate::random::Prng;
use crate::roles::{Rank, RoleName};
use crate::types::{CardSuit, Marker, PlayerId, PlayerName};
use macros::tag::Tag;
use rand::prelude::*;
use std::borrow::{Borrow, BorrowMut, Cow};
use std::fmt::Debug;
use std::iter::repeat;

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
    pub index: u8,
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

pub struct Notification {
    pub player: PlayerIndex,
    pub message: Cow<'static, str>,
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
    pub rng: Prng,
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

    // card specific metadata
    pub museum: Museum,
    pub alchemist: usize,
    pub tax_collector: usize,
}

#[derive(Debug)]
pub struct Characters(pub Vec<GameRole>);

impl Characters {
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

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn next(&self, index: u8) -> Option<u8> {
        if index as usize >= self.0.len() {
            return None;
        }
        return Some(index + 1);
    }

    pub fn index(&self, index: u8) -> &GameRole {
        &self.0[index as usize]
    }

    pub fn index_mut(&mut self, index: u8) -> &mut GameRole {
        &mut self.0[index as usize]
    }

    pub fn get(&self, role_name: RoleName) -> Option<&GameRole> {
        self.0.iter().find(|game_role| game_role.role == role_name)
    }

    pub fn get_mut(&mut self, role_name: RoleName) -> Option<&mut GameRole> {
        self.0
            .iter_mut()
            .find(|game_role| game_role.role == role_name)
    }

    pub fn has_bishop_protection(&self, player: PlayerIndex) -> bool {
        let bishop = match self.get(RoleName::Bishop) {
            Some(bishop) => bishop,
            None => return false,
        };

        // if bishop was killed it is not revealed
        if !bishop.revealed {
            return false;
        }

        // witch takes bishop permission
        if bishop.markers.iter().any(|m| *m == Marker::Bewitched) {
            return self
                .get(RoleName::Witch)
                .is_some_and(|witch| witch.player.is_some_and(|w| w == player));
        }

        bishop.player.is_some_and(|p| p == player)
    }

    pub fn has_tax_collector(&self) -> bool {
        self.get(RoleName::TaxCollector).is_some()
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
        Ok(self.characters.index(call.index))
    }

    pub fn active_role_mut(&mut self) -> Result<&mut GameRole> {
        let call = self.active_turn.call()?;
        Ok(self.characters.index_mut(call.index))
    }

    pub fn start(lobby: Lobby, mut rng: Prng) -> Result<Game> {
        let Lobby {
            mut players,
            config,
        } = lobby;

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

        game.begin_draft();
        Ok(game)
    }

    pub fn begin_draft(&mut self) {
        self.round += 1;
        let draft = Draft::begin(
            self.players.len(),
            self.crowned,
            self.characters
                .0
                .iter()
                .map(|game_role| game_role.role)
                .collect(),
            &mut self.rng,
        );
        for c in draft.faceup_discard.iter() {
            self.characters
                .get_mut(*c)
                .unwrap()
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
                let c = self.characters.index(call.index);
                if self.has_gathered_resources()
                    && c.markers.iter().any(|m| *m == Marker::Bewitched)
                {
                    self.characters
                        .get(RoleName::Witch)
                        .and_then(|game_role| game_role.player)
                        .ok_or("No witch!".into())
                } else {
                    c.player
                        .ok_or(format!("No role at index {} in the roster!", call.index).into())
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
            }

            Turn::Call(Call {
                end_of_round: true, ..
            }) => {
                if self.active_role().unwrap().role != RoleName::Emperor {
                    log::error!("What are you doing man?");
                    vec![]
                } else if self.active_perform_count(ActionTag::EmperorGiveCrown) == 0 {
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
                let c = self.characters.index(call.index);
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
        } = perform_action(self, &action)?;

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
                    self.players[self.characters.get(RoleName::Witch).unwrap().player.unwrap().0].name
                )
                .into(),
            );
        }

        if c.markers.iter().any(|m| *m == Marker::Robbed) {
            let player = self.players[c.player.unwrap().0].borrow_mut();
            let gold = player.gold;
            player.gold = 0;
            let thief = self
                .characters
                .get(RoleName::Thief)
                .unwrap()
                .player
                .unwrap();

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

    pub fn discard_district(&mut self, district: DistrictName) {
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

    pub fn complete_build(&mut self, player: PlayerIndex, spent: usize, district: DistrictName) {
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

    pub fn after_gather_resources(&self) -> Option<Followup> {
        log::info!("After gathering, is there a forced followup?");
        self.forced_to_gather_resources()
            .and_then(|reason| match reason {
                ForcedToGatherReason::Witch => Some(Followup::Bewitch),
                ForcedToGatherReason::Bewitched => None,
                ForcedToGatherReason::Blackmailed => Some(Followup::HandleBlackmail),
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

    pub fn remove_first<T: PartialEq>(items: &mut Vec<T>, item: T) -> Option<T> {
        let index = items
            .iter()
            .enumerate()
            .find_map(|(i, v)| if item == *v { Some(i) } else { None })?;
        Some(items.remove(index))
    }

    pub fn gain_cards(&mut self, amount: usize) -> usize {
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

    pub fn gain_gold_for_suit(&mut self, suit: CardSuit) -> ActionResult {
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

    pub fn gain_cards_for_suit(&mut self, suit: CardSuit) -> Result<ActionOutput> {
        let player = self.active_player()?;
        let count = player.count_suit_for_resource_gain(suit);

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
                    index: 0,
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
                            index: 0,
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
                let o = self.characters.next(call.index);
                if let Some(rank) = o {
                    self.active_turn = Turn::Call(Call {
                        index: rank,
                        end_of_round: false,
                    });
                } else if let Some((i, _)) = self
                    .characters
                    .0
                    .iter()
                    .enumerate()
                    .find(|(i, game_role)| game_role.role == RoleName::Emperor)
                {
                    self.active_turn = Turn::Call(Call {
                        index: i as u8,
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
        // Redo: queen action
        // If the queen has not already done their queen action, they get gold at end of round for
        // being adjacent to royalty
        //
        //let queen = self.characters.get(RoleName::Queen);
        //let n = self.players.len();
        //let p2 = index;
        //if self. queen.is_some_and(|q| {
        //    q.player
        //        .is_some_and(|p1| ((p1.0 + 1) % n == p2.0 || (p2.0 + 1) % n == p1.0))
        //}) {
        //    self.players[queen.unwrap().player.unwrap().0].gold += 3;
        //    self.logs.push(
        //        format!(
        //            "The Queen ({}) was seated next to royalty ({}); they gain 3 gold.",
        //            self.players[queen.unwrap().player.unwrap().0].name,
        //            royalty.unwrap().role.display_name()
        //        )
        //        .into(),
        //    );
        //}

        let heir = self.characters.0.iter().find(|c| {
            c.markers.iter().any(|m| *m == Marker::Killed)
                && (c.role == RoleName::King || c.role == RoleName::Patrician)
        });

        if heir.is_some() {
            self.crowned = heir.unwrap().player.unwrap();
            self.logs.push(
                format!(
                    "{}'s heir {} crowned.",
                    heir.unwrap().role.display_name(),
                    self.players[self.crowned.0].name
                )
                .into(),
            );
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
        for character in self.characters.0.iter_mut() {
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
        assert_eq!(e21deck.draw(), Some(5));
        assert_eq!(deck.draw(), None);
    }
}
