use std::fmt::Debug;

use crate::game::Game;
use crate::random::Seed;

use crate::{game, lobby};

use rusqlite::{Connection, Result};

use crate::actions::Action;

pub struct DbLog {
    conn: Connection,
    game_id: usize,
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
    pub fn new(seed: Seed, players: &[lobby::Player]) -> game::Result<Self> {
        let path = format!("{}/volume/games.db", env!("CARGO_MANIFEST_DIR"));
        let players = serde_json::to_string(players).map_err(|e| e.to_string())?;
        let conn = Connection::open(path).map_err(|e| e.to_string())?;
        let game_id: usize = conn
            .prepare("INSERT INTO games (seed, players) VALUES (?1, ?2) RETURNING (id)")
            .map_err(|e| e.to_string())?
            .query_row((seed, players), |row| row.get("id"))
            .map_err(|e| e.to_string())?;
        log::info!("game_id: {}", game_id);
        Ok(Self { game_id, conn })
    }

    pub fn append(&mut self, action: &Action) -> Result<()> {
        self.conn
            .prepare_cached("INSERT INTO actions (game_id, data) VALUES (?1, ?2)")?
            .execute((self.game_id, serde_json::to_string(action).unwrap()))?;
        Ok(())
    }

    pub fn restore(_game_id: &str) -> game::Result<Game> {
        Err("not implemented".into())
        /*
        let path = format!("{}/volume/games.db", env!("CARGO_MANIFEST_DIR"));
        let conn = Connection::open(path).unwrap();
        let (seed, players): (Seed, String, String) = conn
            .prepare("SELECT seed, players FROM games WHERE games.id = ?1")
            .map_err(|e| e.to_string())?
            .query_row([game_id], |row| Ok((row.get("seed")?, row.get("players")?)))
            .map_err(|e| e.to_string())?;

        let players: Vec<lobby::Player> =
            serde_json::from_str(&players).map_err(|e| e.to_string())?;

        let rng = Prng::from_seed(seed);
        let mut game = Game::start(rng, lobby, rng);
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

        // disable db_log while replaying the game up to the current point.
        let db_log = game.db_log.take();
        for action in actions {
            log::info!("{:?}", action);
            game.perform(action)?;
        }
        game.db_log = db_log;

        Ok(game)
        */
    }
}
