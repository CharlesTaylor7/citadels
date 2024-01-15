use std::fmt::Debug;

use crate::game::Game;
use crate::random::{Prng, Seed};
use crate::{game, lobby};
use rand_core::SeedableRng;
use rusqlite::{Connection, Result, Statement};

use crate::actions::Action;

pub struct DbLog {
    conn: Connection,
    game_id: String,
}
impl Debug for DbLog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "db_log omitted")
    }
}

impl DbLog {
    // https://www.sqlite.org/docs.html
    // https://www.sqlite.org/wal.html
    // https://news.ycombinator.com/item?id=33975635
    // https://github.com/rusqlite/rusqlite
    pub fn new(players: &[lobby::Player], seed: Seed) -> game::Result<Self> {
        let path = format!("{}/volume/games.db", env!("CARGO_MANIFEST_DIR"));

        let players = serde_json::to_string(players).map_err(|e| e.to_string())?;
        let conn = Connection::open(path).unwrap();
        let game_id: String = conn
            .prepare("INSERT INTO games (seed, players) VALUES (?1, ?2) RETURNING (id)")
            .map_err(|e| e.to_string())?
            .query_row((seed, players), |row| row.get("id"))
            .map_err(|e| e.to_string())?;

        Ok(Self { game_id, conn })
    }

    fn append(&mut self, game_id: &str, action: &Action) -> Result<()> {
        self.conn
            .prepare_cached("INSERT INTO actions (game_id, data) VALUES (?1, ?2)")?
            .execute((game_id, serde_json::to_string(action).unwrap()))?;
        Ok(())
    }

    fn restore(game_id: &str) -> game::Result<Game> {
        let path = format!("{}/volume/games.db", env!("CARGO_MANIFEST_DIR"));
        let conn = Connection::open(path).unwrap();
        let (players, seed): (String, Seed) = conn
            .prepare("SELECT seed FROM games WHERE games.id = ?1")
            .map_err(|e| e.to_string())?
            .query_row([game_id], |row| Ok((row.get("players")?, row.get("seed")?)))
            .map_err(|e| e.to_string())?;

        let players: Vec<lobby::Player> =
            serde_json::from_str(&players).map_err(|e| e.to_string())?;

        let rng = Prng::from_seed(seed);
        let mut game = Game::start(lobby::Lobby { players }, rng);
        let actions: Vec<Action> = conn
            .prepare("SELECT data FROM actions WHERE actions.game_id = ?1")
            .map_err(|e| e.to_string())?
            .query_map([game_id], |row| row.get("data"))
            .map_err(|e| e.to_string())?
            .map(|result| {
                let data: String = result.map_err(|e| e.to_string())?;
                serde_json::from_str(&data).map_err(|e| e.to_string())?
            })
            .collect::<game::Result<Vec<Action>>>()?;

        for action in actions {
            game.perform(action)?;
        }

        Ok(game)
    }
}
