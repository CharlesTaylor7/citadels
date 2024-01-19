use crate::templates::filters;
use crate::templates::{DistrictTemplate, RoleTemplate};
use crate::{
    data::characters::ROLES,
    districts::{DistrictName, UNIQUE},
    lobby::{ConfigOption, Player},
    roles::{Rank, RoleName},
};
use askama::Template;
use std::collections::{HashMap, HashSet};

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
#[template(path = "lobby/config/districts.html")]
pub struct DistrictConfigTemplate<'a> {
    pub selected: &'static str,
    pub array: String,
    pub districts: Vec<(usize, ConfigOption, DistrictTemplate<'a>)>,
}

#[derive(Template)]
#[template(path = "lobby/config/roles.html")]
pub struct RoleConfigTemplate<'a> {
    pub unit: &'a (),
    pub selected: &'static str,
    pub cols: Vec<ConfigColumn>,
}

pub struct ConfigColumn {
    pub invalid: bool,
    pub roles: Vec<(bool, RoleTemplate)>,
}

impl<'a> RoleConfigTemplate<'a> {
    pub fn from_config(config: &'a HashSet<RoleName>, invalid: &'a HashSet<Rank>) -> Self {
        Self {
            unit: &(),
            selected: "Roles",
            cols: ROLES
                .chunks(3)
                .map(|chunk| ConfigColumn {
                    invalid: invalid.contains(&chunk[0].rank),
                    roles: chunk
                        .iter()
                        .map(|c| (config.contains(&c.name), RoleTemplate::from(c.name, 200.0)))
                        .collect::<Vec<_>>(),
                })
                .collect(),
        }
    }
}

impl<'a> DistrictConfigTemplate<'a> {
    pub fn from_config(config: &'a HashMap<DistrictName, ConfigOption>) -> Self {
        Self {
            selected: "Districts",
            array: serde_json::to_string(&["Sometimes", "Always", "Never"]).unwrap(),
            districts: UNIQUE
                .into_iter()
                .map(|d| {
                    let option = config.get(&d.name).cloned().unwrap_or_default();
                    (option as usize, option, DistrictTemplate::from(d.name))
                })
                .collect(),
        }
    }
}
