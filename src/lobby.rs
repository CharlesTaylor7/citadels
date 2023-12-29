type PlayerId = String;

#[derive(serde::Serialize, Clone)]
pub struct Player {
    pub id: PlayerId,
    pub name: String,
}

pub struct Lobby {
    pub players: Vec<Player>,
}

impl Lobby {
    pub fn register(&mut self, id: &str, name: &str) {
        match self.players.iter_mut().find(|p| p.id == id) {
            Some(p) => {
                p.name = name.to_owned();
            }
            None => {
                self.players.push(Player {
                    id: id.to_owned(),
                    name: name.to_owned(),
                });
            }
        }
    }

    pub fn new(players: Vec<Player>) -> Lobby {
        Lobby { players }
    }
}

impl Default for Lobby {
    fn default() -> Self {
        Self {
            players: Vec::new(),
        }
    }
}
