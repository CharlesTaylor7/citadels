use std::collections::HashMap;

use crate::{
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
pub enum RoleConfig {
    #[default]
    Sometimes,
    Never,
    Always,
}

pub struct GameConfig {
    roles: HashMap<RoleName, RoleConfig>,
}

impl Default for GameConfig {
    fn default() -> Self {
        let mut roles = HashMap::from([
            (RoleName::Magistrate, RoleConfig::Always),
            (RoleName::Blackmailer, RoleConfig::Always),
            (RoleName::TaxCollector, RoleConfig::Always),
            (RoleName::Trader, RoleConfig::Always),
            (RoleName::Navigator, RoleConfig::Always),
        ]);

        for role in RoleName::iter() {
            if !role.enabled() {
                roles.insert(role, RoleConfig::Never);
            }
        }

        Self { roles }
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
            roles.insert(r, RoleConfig::Always);
        }
        Self { roles }
    }
    pub fn config(&self, role: &RoleName) -> RoleConfig {
        self.roles.get(role).map_or(RoleConfig::default(), |r| *r)
    }

    /// If multiple roles are marked as "Always", either of them could be picked.
    /// if all roles in a row are marked "never", the game state is invalid.
    /// Its up to the server route to check for these error states, when the user updates their
    /// config.
    /// Here its assumed the config is valid.
    pub fn select<'a, T: RngCore>(
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
                match self.config(&r.name) {
                    RoleConfig::Always => {
                        let group = &mut grouped_by_rank[r.rank.to_index()];
                        group.clear();
                        group.push(r.name)
                    }
                    RoleConfig::Sometimes => {
                        let group = &mut grouped_by_rank[r.rank.to_index()];
                        let is_locked = group
                            .get(0)
                            .is_some_and(|r| self.config(r) == RoleConfig::Always);

                        if !is_locked {
                            group.push(r.name);
                        }
                    }
                    RoleConfig::Never => {}
                }
            }
        }

        grouped_by_rank
            .into_iter()
            .filter_map(|roles| roles.choose(rng).cloned())
    }
}
