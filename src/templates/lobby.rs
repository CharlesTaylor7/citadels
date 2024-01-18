use crate::{
    data::characters::ROLES,
    lobby::{ConfigOption, GameConfig, Player},
    roles::{Rank, RoleName},
};
use askama::Template;

use super::RoleTemplate;

#[derive(Template)]
#[template(path = "lobby/index.html")]
pub struct LobbyTemplate<'a> {
    pub username: &'a str,
    pub players: &'a [Player],
}

#[derive(Template)]
#[template(path = "lobby/players.html")]
pub struct LobbyPlayersTemplate<'a> {
    pub players: &'a [Player],
}

#[derive(Template)]
#[template(path = "lobby/config.html")]
pub struct ConfigTemplate<'a> {
    pub unit: &'a (),
    pub rows: Vec<Vec<(bool, RoleTemplate)>>,
}

impl<'a> ConfigTemplate<'a> {
    pub fn from_config(config: &'a GameConfig) -> Self {
        ConfigTemplate {
            unit: &(),
            rows: ROLES
                .chunks(3)
                .map(|chunk| {
                    chunk
                        .iter()
                        .map(|c| (config.role(&c.name), RoleTemplate::from(c.name, 200.0)))
                        .collect::<Vec<_>>()
                })
                .collect(),
        }
    }
}
