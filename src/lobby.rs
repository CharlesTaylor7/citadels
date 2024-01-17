use std::{
    collections::HashMap, hash::Hasher, intrinsics::atomic_cxchgweak_acqrel_seqcst, ops::Deref,
};

use crate::{
    districts::{self, DistrictName},
    game::{Characters, GameRole},
    roles::RoleName,
    types::{PlayerId, PlayerName},
};
use rand::seq::SliceRandom;
use rand_core::RngCore;
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct Player {
    pub id: PlayerId,
    pub name: PlayerName,
}

#[derive(Default)]
pub struct Lobby {
    pub players: Vec<Player>,
    pub config: GameConfig,
}

impl Lobby {
    pub fn demo(players: Vec<&str>) -> Self {
        Self {
            config: GameConfig::default(),
            players: players
                .into_iter()
                .enumerate()
                .map(|(i, p)| Player {
                    id: format!("{}", i + 1),
                    name: PlayerName::from(p.to_owned()),
                })
                .collect(),
        }
    }
    pub fn register(&mut self, id: &str, name: &str) {
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
    }
}

#[derive(Default, PartialEq, Eq, Clone, Copy)]
pub enum ConfigOption {
    #[default]
    Sometimes,
    Never,
    Always,
}

pub struct GameConfig {
    roles: HashMap<RoleName, ConfigOption>,
    districts: HashMap<DistrictName, ConfigOption>,
}

impl Default for GameConfig {
    fn default() -> Self {
        let mut roles = HashMap::from([
            (RoleName::Magistrate, ConfigOption::Always),
            (RoleName::Blackmailer, ConfigOption::Always),
            (RoleName::TaxCollector, ConfigOption::Always),
            (RoleName::Trader, ConfigOption::Always),
            (RoleName::Navigator, ConfigOption::Always),
        ]);

        for role in RoleName::iter() {
            if !role.enabled() {
                roles.insert(role, ConfigOption::Never);
            }
        }

        let mut districts = HashMap::from([(DistrictName::Museum, ConfigOption::Always)]);

        for district in districts::UNIQUE {
            if !district.name.enabled() {
                districts.insert(district.name, ConfigOption::Never);
            }
        }

        Self { roles, districts }
    }
}

impl GameConfig {
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
        let mut roles = HashMap::with_capacity(9);
        for r in base {
            roles.insert(r, ConfigOption::Always);
        }
        Self {
            roles,
            districts: HashMap::default(),
        }
    }
    pub fn role(&self, role: &RoleName) -> ConfigOption {
        self.roles.get(role).map_or(ConfigOption::default(), |r| *r)
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
    ) -> impl Iterator<Item = RoleName> + 'a {
        // 9th rank is disallowed for 2
        // 9th rank is required for 3
        // 9th rank is optional for 4-7
        // 9th rank is required for 8
        let n = if num_players == 2 { 8 } else { 9 };
        log::info!("Selecting {} roles for {} player game", n, num_players);
        let mut grouped_by_rank = vec![Vec::with_capacity(3); n];

        for r in crate::data::characters::ROLES {
            if num_players >= r.name.min_player_count() {
                match self.role(&r.name) {
                    ConfigOption::Always => {
                        let group = &mut grouped_by_rank[r.rank.to_index()];
                        group.clear();
                        group.push(r.name)
                    }
                    ConfigOption::Sometimes => {
                        let group = &mut grouped_by_rank[r.rank.to_index()];
                        let is_locked = group
                            .get(0)
                            .is_some_and(|r| self.role(r) == ConfigOption::Always);

                        if !is_locked {
                            group.push(r.name);
                        }
                    }
                    ConfigOption::Never => {}
                }
            }
        }

        grouped_by_rank.into_iter().map(|roles| {
            *roles
                .choose(rng)
                .expect("there should be a role for each rank")
        })
    }

    pub fn select_unique_districts<T: RngCore>(
        &self,
        rng: &mut T,
    ) -> impl Iterator<Item = DistrictName> + '_ {
        let mut always = Vec::with_capacity(14);
        let mut sometimes = Vec::with_capacity(14);
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
