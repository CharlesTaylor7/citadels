use crate::{
    districts::DistrictName,
    game,
    roles::{Rank, RoleName},
    types::{PlayerId, PlayerName},
};
use rand::seq::SliceRandom;
use rand_core::RngCore;
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Player {
    pub id: PlayerId,
    pub name: PlayerName,
}

impl Player {
    pub fn demo(name: &'static str) -> Self {
        Self {
            id: name.into(),
            name: name.into(),
        }
    }
}

#[derive(Default, Clone, Debug, Serialize)]
pub struct Lobby {
    pub players: Vec<Player>,
    pub config: GameConfig,
}

impl Lobby {
    pub fn demo(count: usize) -> Self {
        let players = vec![
            "Alph",
            "Brittany",
            "Charlie",
            "Dana",
            "Eli",
            "Francesca",
            "George",
            "Helen",
        ];
        Self {
            config: GameConfig::default(),
            players: players
                .into_iter()
                .take(count)
                .enumerate()
                .map(|(i, p)| Player {
                    id: format!("{}", i + 1),
                    name: PlayerName::from(p.to_owned()),
                })
                .collect(),
        }
    }

    pub fn register(&mut self, id: &str, name: &str) -> game::Result<()> {
        if self
            .players
            .iter()
            .any(|p| p.id != id && p.name.borrow() as &str == name)
        {
            return Err("username taken".into());
        }
        match self.players.iter_mut().find(|p| p.id == id) {
            Some(p) => {
                p.name = PlayerName::from(name.to_owned());
            }
            None => {
                self.players.push(Player {
                    id: id.to_owned(),
                    name: PlayerName::from(name.to_owned()),
                });
            }
        }
        Ok(())
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
#[repr(u8)]
pub enum ConfigOption {
    #[default]
    Sometimes,
    Always,
    Never,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GameConfig {
    pub role_anarchy: bool,
    pub roles: HashSet<RoleName>,
    pub districts: HashMap<DistrictName, ConfigOption>,
}

impl Default for GameConfig {
    fn default() -> Self {
        let mut roles = HashSet::new();
        for role in RoleName::iter() {
            roles.insert(role);
        }

        let districts = HashMap::new();

        Self {
            role_anarchy: false,
            roles,
            districts,
        }
    }
}

impl GameConfig {
    pub fn set_roles(
        &mut self,
        roles: HashSet<RoleName>,
    ) -> Result<(), (HashSet<RoleName>, HashSet<Rank>)> {
        let mut error_ranks = HashSet::with_capacity(9);
        for chunk in crate::data::characters::ROLES.chunks(3) {
            if chunk.iter().all(|c| !roles.contains(&c.name)) {
                error_ranks.insert(chunk[0].rank);
            }
        }
        if error_ranks.len() > 0 {
            return Err((roles, error_ranks));
        }
        self.roles = roles;
        Ok(())
    }

    pub fn base_set() -> Self {
        let base = [
            RoleName::Assassin,
            RoleName::Thief,
            RoleName::Magician,
            RoleName::King,
            RoleName::Bishop,
            RoleName::Merchant,
            RoleName::Architect,
            RoleName::Warlord,
            RoleName::Artist,
        ];
        let mut roles = HashSet::with_capacity(9);
        for r in base {
            roles.insert(r);
        }
        Self {
            role_anarchy: false,
            roles,
            districts: HashMap::default(),
        }
    }
    pub fn role_enabled(&self, role: &RoleName) -> bool {
        self.roles.contains(role)
    }

    pub fn district(&self, district: &DistrictName) -> ConfigOption {
        self.districts
            .get(district)
            .map_or(ConfigOption::default(), |r| *r)
    }

    /// If multiple roles are marked as "Always", either of them could be picked.
    /// if all roles in a row are marked "never", the game state is invalid.
    /// Its up to the server route to check for these error states, when the user updates their
    /// config.
    /// Here its assumed the config is valid.
    pub fn select_roles<'a, T: RngCore>(
        &self,
        rng: &'a mut T,
        num_players: usize,
    ) -> game::Result<Vec<RoleName>> {
        // 9th rank is disallowed for 2
        // 9th rank is required for 3
        // 9th rank is optional for 4-7
        // 9th rank is required for 8
        let role_count = if num_players == 2 { 8 } else { 9 };
        log::info!(
            "Selecting {} roles for {} player game",
            role_count,
            num_players
        );

        if self.role_anarchy {
            let roles: Vec<RoleName> = crate::data::characters::ROLES
                .iter()
                .map(|r| r.name)
                .filter(|r| num_players >= r.min_player_count() && self.role_enabled(r))
                .collect();
            return Ok(roles.choose_multiple(rng, role_count).cloned().collect());
        } else {
            let mut grouped_by_rank = vec![Vec::with_capacity(3); role_count];

            for r in crate::data::characters::ROLES {
                if num_players >= r.name.min_player_count() && self.role_enabled(&r.name) {
                    grouped_by_rank[r.rank.to_index()].push(r.name);
                }
            }

            return grouped_by_rank
                .into_iter()
                .enumerate()
                .map(|(i, roles)| {
                    roles
                        .choose(rng)
                        .copied()
                        .ok_or(format!("No enabled roles for rank {}", i + 1).into())
                })
                .collect();
        }
    }

    pub fn select_unique_districts<T: RngCore>(
        &self,
        rng: &mut T,
    ) -> impl Iterator<Item = DistrictName> + '_ {
        let mut always = Vec::with_capacity(14);
        let mut sometimes = Vec::with_capacity(30);
        for d in crate::districts::UNIQUE {
            match self.district(&d.name) {
                ConfigOption::Always => always.push(d.name),
                ConfigOption::Sometimes => sometimes.push(d.name),
                ConfigOption::Never => {}
            }
        }

        if always.len() < 14 {
            sometimes.shuffle(rng);
        }
        always.into_iter().chain(sometimes).take(14)
    }
}
