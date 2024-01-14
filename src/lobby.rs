use crate::types::{PlayerId, PlayerName};
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct Player {
    pub id: PlayerId,
    pub name: PlayerName,
}

#[derive(Default)]
pub struct Lobby {
    pub players: Vec<Player>,
}

impl Lobby {
    pub fn demo(players: Vec<&str>) -> Self {
        Self {
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
