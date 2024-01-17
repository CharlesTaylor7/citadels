use crate::actions::{Action, ActionTag, CityDistrictTarget, MagicianAction, Resource};
use crate::districts::DistrictName;
use crate::lobby::{self, Lobby};
use crate::random::Prng;
use crate::roles::{Rank, RoleName};
use crate::sqlite::DbLog;
use crate::types::{CardSuit, Marker, PlayerId, PlayerName};
use macros::tag::Tag;
use rand::prelude::*;
use std::borrow::{Borrow, BorrowMut, Cow};
use std::fmt::Debug;

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Turn {
    GameOver,
    Draft(PlayerIndex),
    Call(Rank),
}

impl Turn {
    pub fn draft(&self) -> Option<PlayerIndex> {
        if let Turn::Draft(index) = self {
            Some(*index)
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
pub struct FollowupAction {
    pub action: ActionTag,
    pub revealed: Vec<DistrictName>,
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
    pub followup: Option<FollowupAction>,
    pub log: Cow<'static, str>,
}

#[derive(Debug)]
pub enum ResponseAction {
    Warrant,
    Blackmail,
}

#[derive(Debug)]
pub struct Game {
    rng: Prng,
    pub round: usize,
    pub deck: Deck<DistrictName>,
    pub players: Vec<Player>,
    pub characters: Characters,
    pub crowned: PlayerIndex,
    pub active_turn: Turn,
    pub draft: Draft,
    pub followup: Option<FollowupAction>,
    pub pause_for_response: Option<ResponseAction>,
    pub turn_actions: Vec<Action>,
    pub first_to_complete: Option<PlayerIndex>,
    pub logs: Vec<Cow<'static, str>>,
    pub db_log: Option<DbLog>,
    // card specific metadata
    pub museum: Vec<DistrictName>,
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

    pub fn new(roles: impl Iterator<Item = RoleName>) -> Self {
        Self(
            roles
                .map(|role| GameRole {
                    role,
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

    pub fn has_revealed_role(&self, player: &Player, role: RoleName) -> bool {
        player.roles.iter().any(|r| *r == role) && self.get(role.rank()).revealed
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
            .unwrap()
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
                DistrictName::Museum => self.museum.len(),
                DistrictName::Basilica => player
                    .city
                    .iter()
                    .filter(|c| c.name.data().cost % 2 == 1)
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

    pub fn default_game() -> Option<Game> {
        if cfg!(not(feature = "dev")) {
            return None;
        }
        let lobby = Lobby::demo(vec!["Alph", "Brittany", "Charlie"]);
        let roles = lobby
            .config
            .select(&mut rand::thread_rng(), lobby.players.len())
            .collect::<Vec<_>>();

        let mut game = Game::start(lobby.players, roles, SeedableRng::from_entropy());

        // deal out roles randomly
        let mut roles: Vec<_> = game.characters.iter().collect();
        roles.shuffle(&mut game.rng);

        for (i, role) in roles.iter().enumerate() {
            let index = i % 3;
            game.players[index].roles.push(*role);
            game.characters.get_mut(role.rank()).player = Some(PlayerIndex(index));
        }

        for p in game.players.iter_mut() {
            p.roles.sort_by_key(|r| r.rank());

            /*
            for card in game.deck.draw_many(4) {
                p.city.push(CityDistrict {
                    name: card,
                    beautified: false,
                });
            }
            */
        }

        game.active_turn = Turn::Call(Rank::Two);
        game.start_turn().ok()?;

        Some(game)
    }

    pub fn active_role(&self) -> Result<&GameRole> {
        let rank = self.active_turn.call().ok_or("not the call phase")?;
        Ok(self.characters.get(rank))
    }

    pub fn active_role_mut(&mut self) -> Option<&mut GameRole> {
        let rank = self.active_turn.call()?;
        Some(self.characters.get_mut(rank))
    }

    pub fn start(mut players: Vec<lobby::Player>, roles: Vec<RoleName>, mut rng: Prng) -> Game {
        let db_log = DbLog::new(rng.seed, &players, &roles)
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
        let characters = Characters::new(roles.into_iter());
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
            pause_for_response: None,
            draft: Draft::default(),
            deck: Deck::new(deck),
            active_turn: Turn::Draft(crowned),
            logs: Vec::new(),
            followup: None,
            turn_actions: Vec::new(),
            museum: Vec::new(),
            first_to_complete: None,
        };
        game.begin_draft();
        game
    }

    pub fn begin_draft(&mut self) {
        self.round += 1;
        self.active_turn = Turn::Draft(self.crowned);
        self.draft.remaining = self.characters.iter().collect();

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

    pub fn active_player_index(&self) -> Result<PlayerIndex> {
        match &self.active_turn {
            Turn::GameOver => Err("game over".into()),
            Turn::Draft(index) => Ok(*index),
            Turn::Call(rank) => self
                .characters
                .get(*rank)
                .player
                .ok_or(format!("no player with rank {}", rank).into()),
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

    pub fn allowed_actions(&self) -> Vec<ActionTag> {
        if let Some(f) = &self.followup {
            return vec![f.action];
        }

        match self.active_turn {
            Turn::GameOver => {
                vec![]
            }
            Turn::Draft(_) => {
                if self.active_perform_count(ActionTag::DraftPick) == 0 {
                    vec![ActionTag::DraftPick]

                    // the first player to draft in the two player game does not discard.
                    // the last pick is between two cards.
                    // the one they don't pick is automatically discarded.
                    // So really only the middle two draft turns should show the discard button
                } else if self.players.len() == 2
                    && (self.draft.remaining.len() == 5 || self.draft.remaining.len() == 3)
                {
                    vec![ActionTag::DraftDiscard]
                } else {
                    vec![]
                }
            }

            Turn::Call(rank) => {
                let mut actions = Vec::new();
                let c = self.characters.get(rank);

                for (n, action) in c.role.data().actions {
                    if self.active_perform_count(*action) < *n {
                        actions.push(*action);
                    }
                }

                for card in self.active_player().iter().flat_map(|p| &p.city) {
                    if let Some(action) = card.name.action() {
                        if self.active_perform_count(action) < 1 {
                            actions.push(action);
                        }
                    }
                }

                // You have to gather resources before building
                if self
                    .turn_actions
                    .iter()
                    .all(|act| !act.tag().is_resource_gathering())
                {
                    // gather
                    actions.push(ActionTag::GatherResourceGold);
                    actions.push(ActionTag::GatherResourceCards);
                } else {
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

    pub fn perform(&mut self, action: Action) -> Result<()> {
        self.active_player()?;
        let allowed = self.allowed_actions();
        if !allowed.contains(&action.tag()) {
            return Err("Action is not allowed".into());
        }

        let ActionOutput { followup, log } = self.perform_action(&action)?;

        if let Some(log) = self.db_log.as_mut() {
            if let Err(err) = log.append(&action) {
                log::error!("{}", err);
                log::info!("Disabling db action log");
                self.db_log = None;
            }
        }

        self.followup = followup;

        let tag = action.tag();
        log::info!("{:#?}", log);
        if self.followup.is_some() {
            log::info!("followup: {:#?}", self.followup);
        }
        self.turn_actions.push(action.clone());
        if let Some(role) = self.active_role_mut() {
            role.logs.push(log.into());
        }

        if tag == ActionTag::EndTurn {
            self.end_turn()?;
        }

        Ok(())
    }

    fn start_turn(&mut self) -> Result<()> {
        let c = self.active_role_mut();
        if c.is_none() {
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
            c.logs.push("No one responded".into());

            *c_ref = c;
            self.call_next();
            return self.start_turn();
        }
        c.revealed = true;
        let player = self.players[c.player.unwrap().0].borrow_mut();
        c.logs
            .push(format!("{} started their turn.", player.name).into());

        if c.markers.iter().any(|m| *m == Marker::Robbed) {
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
            let mut to_discard = std::mem::replace(&mut self.museum, vec![]);
            to_discard.push(DistrictName::Museum);
            to_discard.shuffle(&mut self.rng);
            for card in to_discard {
                self.deck.discard_to_bottom(card);
            }
        } else {
            self.deck.discard_to_bottom(district);
        }
    }

    fn perform_action(&mut self, action: &Action) -> ActionResult {
        Ok(match action {
            Action::DraftPick { role } => {
                let index = self.active_turn.draft().ok_or("not the draft")?;

                Game::remove_first(&mut self.draft.remaining, *role);
                let c = self.characters.get_mut(role.rank());
                c.player = Some(index);
                let player = self.players[index.0].borrow_mut();
                player.roles.push(*role);

                ActionOutput {
                    log: format!("{} drafted a role.", player.name).into(),
                    followup: None,
                }
            }

            Action::DraftDiscard { role } => {
                let i = (0..self.draft.remaining.len())
                    .find(|i| self.draft.remaining[*i] == *role)
                    .ok_or("selected role is not available")?;

                self.draft.remaining.remove(i);
                ActionOutput {
                    log: format!("{} discarded a role face down.", self.active_player()?.name)
                        .into(),
                    followup: None,
                }
            }

            Action::EndTurn => {
                // this is handled after the logs are updated.
                ActionOutput {
                    log: format!("{} ended their turn.", self.active_player()?.name).into(),
                    followup: None,
                }
            }

            Action::GatherResourceGold => {
                let active = self.active_player_index()?;
                let mut amount = 2;
                let log = if self.players[active.0].city_has(DistrictName::GoldMine) {
                    amount += 1;
                    "They gathered 3 gold. (1 extra from their Gold Mine).".into()
                } else {
                    "They gathered 2 gold.".into()
                };

                self.players[active.0].gold += amount;

                ActionOutput {
                    log,
                    followup: None,
                }
            }

            Action::GatherResourceCards => {
                let mut draw_amount = 2;
                if self.active_player()?.city_has(DistrictName::Observatory) {
                    draw_amount += 1;
                }

                let mut drawn = self.deck.draw_many(draw_amount).collect();

                if self.active_player()?.city_has(DistrictName::Library) {
                    self.active_player_mut()?.hand.append(&mut drawn);

                    ActionOutput {
                        log: format!("They gathered cards. With the aid of their library they kept all {} cards.", draw_amount).into(),
                        followup: None,
                    }
                } else {
                    ActionOutput {
                        log: format!(
                            "They revealed {} cards from the top of the deck.",
                            draw_amount
                        )
                        .into(),
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

                Game::remove_first(&mut revealed, *district).ok_or("invalid choice")?;
                self.active_player_mut()?.hand.push(*district);

                revealed.shuffle(&mut self.rng);
                for remaining in revealed {
                    self.deck.discard_to_bottom(remaining);
                }
                ActionOutput {
                    log: "They picked a card.".into(),
                    followup: None,
                }
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
                ActionOutput {
                    log: format!("The Merchant ({}) gained 1 extra gold.", player.name).into(),
                    followup: None,
                }
            }
            Action::ArchitectGainCards => {
                self.gain_cards(2);
                let player = self.active_player()?;
                ActionOutput {
                    log: format!("The Architect ({}) gained 2 extra cards.", player.name).into(),
                    followup: None,
                }
            }
            Action::Build { district } => {
                if self.active_role().unwrap().role == RoleName::Navigator {
                    return Err("The navigator is not allowed to build.".into());
                }

                if self
                    .turn_actions
                    .iter()
                    .all(|a| !a.tag().is_resource_gathering())
                {
                    return Err("Must gather resources before building".into());
                }

                let within_build_limit = *district == DistrictName::Stables
                    || (district.data().suit == CardSuit::Trade
                        && self.active_role().unwrap().role == RoleName::Trader)
                    || self
                        .turn_actions
                        .iter()
                        .filter(|a| a.tag() == ActionTag::Build)
                        .count()
                        < self.active_role().unwrap().role.build_limit();

                if !within_build_limit {
                    return Err(format!(
                        "With your role, you cannot build more than {} time(s), this turn.",
                        self.active_role().unwrap().role.display_name()
                    )
                    .into());
                }
                // build

                let player = self.active_player_mut()?;
                let district = district.data();

                let mut cost = district.cost;
                if district.suit == CardSuit::Unique
                    && player.city.iter().any(|d| d.name == DistrictName::Factory)
                {
                    cost -= 1;
                }

                if cost > player.gold {
                    return Err("not enough gold".into());
                }

                if !player.city_has(DistrictName::Quarry)
                    && player.city.iter().any(|d| d.name == district.name)
                {
                    return Err("cannot build duplicate".into());
                }

                if district.name == DistrictName::Monument && player.city.len() >= 5 {
                    return Err("You can only build the Monument, if you have less than 5 districts in your city".into());
                }

                Game::remove_first(&mut player.hand, district.name).ok_or("card not in hand")?;
                player.gold -= cost;
                player.city.push(CityDistrict::from(district.name));
                if self.active_role().unwrap().role == RoleName::Alchemist {
                    self.alchemist += cost;
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

                // trigger end game
                let player = self.active_player()?;
                if player.city_size() >= self.complete_city_size()
                    && self.first_to_complete.is_none()
                {
                    log::info!("{} is the first to complete their city", player.name);
                    self.first_to_complete = Some(player.index);
                }

                if self.active_role().unwrap().has_warrant() {
                    self.pause_for_response = Some(ResponseAction::Warrant);
                }

                ActionOutput {
                    log: format!("They built a {}.", district.display_name).into(),
                    followup: None,
                }
            }

            Action::TakeCrown => {
                self.crowned = self.active_player_index()?;

                ActionOutput {
                    log: "They took the crown.".into(),
                    followup: None,
                }
            }

            Action::Assassinate { role } => {
                if role.rank() == Rank::One {
                    return Err("cannot kill self".into());
                }
                let target = self.characters.get_mut(role.rank());
                target.markers.push(Marker::Killed);

                ActionOutput {
                    log: format!(
                        "The Assassin ({}) killed the {}; Their turn will be skipped.",
                        self.active_player()?.name,
                        role.display_name(),
                    )
                    .into(),
                    followup: None,
                }
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

                ActionOutput {
                    log: format!(
                        "The Thief ({}) robbed the {}; Their gold will be taken at the start of their turn.",
                        self.active_player()?.name,
                        role.display_name(),
                    ).into(),
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
                    return Err("invalid target".into());
                };

                let hand_count = hand.len();
                let target_count = target.hand.len();

                std::mem::swap(&mut hand, &mut target.hand);
                self.active_player_mut()?.hand = hand;

                ActionOutput {
                    log: format!(
                        "They swapped their hand of {} cards with {}'s hand of {} cards.",
                        hand_count, player, target_count,
                    )
                    .into(),
                    followup: None,
                }
            }

            Action::Magic(MagicianAction::TargetDeck { district }) => {
                let active = self.active_player_mut()?;
                let discard = district.to_vec();
                let backup = active.hand.clone();

                for card in discard.iter() {
                    if Game::remove_first(&mut active.hand, *card).is_none() {
                        active.hand = backup;
                        return Err("Can't discard a card that's not in your hand".into());
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
                    )
                    .into(),
                    followup: None,
                }
            }

            Action::WarlordDestroy { district: target } => {
                if target.district == DistrictName::Keep {
                    return Err("cannot destroy the Keep".into());
                }

                let available_gold = self.active_player()?.gold;
                let complete_size = self.complete_city_size();
                let targeted_player = self
                    .players
                    .iter_mut()
                    .find(|p| {
                        p.name == target.player
                            && !self.characters.has_revealed_role(p, RoleName::Bishop)
                            && p.city_size() < complete_size
                    })
                    .ok_or("invalid player target")?;

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

                let mut destroy_cost = target.district.data().cost - 1;
                if target.beautified {
                    destroy_cost += 1;
                }
                if targeted_player.city_has(DistrictName::GreatWall) {
                    destroy_cost += 1;
                }

                if available_gold < destroy_cost {
                    return Err("not enough gold to destroy".into());
                }

                targeted_player.city.remove(city_index);
                self.active_player_mut()?.gold -= destroy_cost;
                self.discard_district(target.district);

                ActionOutput {
                    log: format!(
                        "The Warlord ({}) destroyed {}'s {}.",
                        self.active_player()?.name,
                        target.player,
                        target.district.data().display_name,
                    )
                    .into(),
                    followup: None,
                }
            }

            Action::Armory { district: target } => {
                if target.district == DistrictName::Keep {
                    return Err("cannot destroy the Keep".into());
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
                //Game::remove_first(&mut
                let (city_index, _) = active_player
                    .city
                    .iter()
                    .enumerate()
                    .find(|(_, d)| d.name == DistrictName::Armory)
                    .unwrap();

                active_player.city.remove(city_index);
                self.deck.discard_to_bottom(DistrictName::Armory);

                self.discard_district(target.district);

                ActionOutput {
                    log: format!(
                        "They sacrificed their Armory to destroy {}'s {}.",
                        target.player,
                        target.district.data().display_name,
                    )
                    .into(),
                    followup: None,
                }
            }

            Action::Beautify {
                district: CityDistrictTarget { district, .. },
            } => {
                let player = self.active_player_mut()?;

                if player.gold < 1 {
                    return Err("not enough gold".into());
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
                    )
                    .into(),
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
                    )
                    .into(),
                    followup: None,
                }
            }

            Action::NavigatorGain {
                resource: Resource::Gold,
            } => {
                let player = self.active_player_mut()?;
                player.gold += 4;

                ActionOutput {
                    log: format!("The Navigator ({}) gained 4 extra gold.", player.name).into(),
                    followup: None,
                }
            }

            Action::SeerTake { .. } => return Err("Not implemented".into()),

            Action::SeerDistribute { .. } => return Err("Not implemented".into()),

            Action::WizardPeek { .. } => return Err("Not implemented".into()),
            Action::WizardPick { .. } => return Err("Not implemented".into()),

            Action::ResourcesFromReligion { gold, cards } => {
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

                ActionOutput {
                    followup: None,
                    log: format!(
                        "The Abbot ({}) gained {} gold and {} cards from their Religious districts",
                        player.name, gold, cards
                    )
                    .into(),
                }
            }
            Action::CollectTaxes => {
                let taxes = self.tax_collector;
                self.active_player_mut()?.gold += taxes;
                self.tax_collector = 0;
                ActionOutput {
                    log: format!("The Tax Collector collects {} gold in taxes.", taxes).into(),
                    followup: None,
                }
            }
            Action::EmperorGiveCrown { .. } => return Err("Not implemented".into()),
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
                ActionOutput {
                    log: log.into(),
                    followup: None,
                }
            }

            Action::Spy { .. } => return Err("Not implemented".into()),
            Action::Bewitch { .. } => return Err("Not implemented".into()),
            Action::Seize { .. } => return Err("Not implemented".into()),
            Action::TakeFromRich { .. } => return Err("Not implemented".into()),
            Action::SendWarrants { signed, unsigned } => {
                let mut roles = Vec::with_capacity(3);
                roles.push(signed);
                for role in unsigned {
                    if roles.iter().any(|r| *r == role) {
                        return Err("cannot assign duplicate warrants".into());
                    }
                    roles.push(role);
                }
                if roles.iter().any(|role| role.rank() == Rank::One) {
                    return Err("cannot assign warrant to self".into());
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
                ActionOutput {
                    log: format!(
                        "Magistrate sends warrants to the {}, the {}, and the {}.",
                        roles[0].display_name(),
                        roles[1].display_name(),
                        roles[2].display_name()
                    )
                    .into(),
                    followup: None,
                }
            }
            Action::Blackmail { flowered, unmarked } => {
                if flowered == unmarked {
                    return Err("cannot blackmail the same role twice".into());
                }
                if flowered.rank() < Rank::Three || unmarked.rank() < Rank::Three {
                    return Err("can only blackmail rank 3 or higher".into());
                }

                if self
                    .characters
                    .get(flowered.rank())
                    .markers
                    .iter()
                    .any(|m| *m == Marker::Killed || *m == Marker::Bewitched)
                {
                    return Err("cannot blackmail the killed or bewitched".into());
                }
                if self
                    .characters
                    .get(unmarked.rank())
                    .markers
                    .iter()
                    .any(|m| *m == Marker::Killed || *m == Marker::Bewitched)
                {
                    return Err("cannot blackmail the killed or bewitched".into());
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

                ActionOutput {
                    log: format!(
                        "The Blackmailer sends blackmail to the {} and the {}",
                        roles[0].display_name(),
                        roles[1].display_name(),
                    )
                    .into(),
                    followup: None,
                }
            }
            Action::ExchangeCityDistricts { .. } => return Err("Not implemented".into()),

            Action::Smithy => {
                let active = self.active_player_mut()?;
                if active.gold < 2 {
                    return Err("not enough gold".into());
                }
                active.gold -= 2;
                self.gain_cards(3);
                ActionOutput {
                    log: "At the Smithy, they forged 2 gold into 3 cards".into(),
                    followup: None,
                }
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

                ActionOutput {
                    log: "At the Laboratory, they transmuted 1 card into 2 gold".into(),
                    followup: None,
                }
            }

            Action::Theater { .. } => return Err("Not implemented".into()),

            Action::Museum { district } => {
                let active = self.active_player_mut()?;
                let (index, _) = active
                    .hand
                    .iter()
                    .enumerate()
                    .find(|(_, name)| *name == district)
                    .ok_or("district not in hand")?;
                let card = active.hand.remove(index);
                self.museum.push(card);

                ActionOutput {
                    log: "They tucked a card face down under their Museum.".into(),
                    followup: None,
                }
            }

            Action::ScholarReveal => {
                let drawn = self.deck.draw_many(7).collect::<Vec<_>>();

                ActionOutput {
                    log: format!(
                        "The Scholar ({}) is choosing from the top {} cards of the deck.",
                        self.active_player()?.name,
                        drawn.len(),
                    )
                    .into(),
                    followup: Some(FollowupAction {
                        action: ActionTag::ScholarPick,
                        revealed: drawn,
                    }),
                }
            }

            Action::ScholarPick { district } => {
                let FollowupAction { mut revealed, .. } =
                    self.followup.take().ok_or("action is not allowed")?;

                Game::remove_first(&mut revealed, *district).ok_or("invalid choice")?;
                self.active_player_mut()?.hand.push(*district);

                for remaining in revealed {
                    self.deck.discard_to_bottom(remaining);
                }
                self.deck.shuffle(&mut self.rng);

                ActionOutput {
                    log: format!(
                        "The Scholar ({}) picked a card, discarded the rest and shuffled the deck.",
                        self.active_player()?.name,
                    )
                    .into(),
                    followup: None,
                }
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
        let amount = player.count_suit_for_resource_gain(suit);
        player.gold += amount;

        Ok(ActionOutput {
            followup: None,
            log: format!("They gained {} gold from their {} districts", amount, suit).into(),
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

        Ok(ActionOutput {
            followup: None,
            log: format!("They gained {} cards from their {} districts", amount, suit).into(),
        })
    }

    fn end_turn(&mut self) -> Result<()> {
        log::info!("ending turn");
        self.turn_actions.clear();

        match self.active_turn {
            Turn::GameOver => {}
            Turn::Draft(index) => {
                // advance turn
                let role_count = if self.players.len() <= 3 { 2 } else { 1 };
                self.active_turn = if self.players.iter().all(|p| p.roles.len() == role_count) {
                    Turn::Call(Rank::One)
                } else {
                    let next = PlayerIndex((index.0 + 1) % self.players.len());
                    Turn::Draft(next)
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
            Turn::Call(_) => {
                if let Ok(player) = self.active_player() {
                    let gains_gold = player.gold == 0 && player.city_has(DistrictName::PoorHouse);
                    let gains_cards = player.hand.len() == 0 && player.city_has(DistrictName::Park);
                    let name = self.active_player().unwrap().name.clone();
                    if gains_gold {
                        self.active_player_mut().unwrap().gold += 1;
                        self.active_role_mut()
                            .unwrap()
                            .logs
                            .push(format!("{} gains 1 gold from their Poor House.", name,).into());
                    }

                    if gains_cards {
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
        match self.active_turn {
            Turn::Call(rank) => {
                let o = self.characters.next(rank);
                if let Some(rank) = o {
                    self.active_turn = Turn::Call(rank);
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
                if character.role == RoleName::Emperor {
                    todo!();
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
                                "The Queen is seated next to the dead {}; they gain 3 gold.",
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

        self.draft.clear();
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
