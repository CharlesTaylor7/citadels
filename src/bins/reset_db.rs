use rusqlite::{Connection, Result};
use std::fs;
use std::fs::OpenOptions;
use std::path::Path;

// https://www.sqlite.org/docs.html
// https://www.sqlite.org/wal.html
// https://news.ycombinator.com/item?id=33975635
fn main() -> Result<()> {
    let path = format!("{}/volume/games.db", env!("CARGO_MANIFEST_DIR"));
    let path = Path::new(&path);
    if path.exists() {
        fs::remove_file(path).unwrap();
    }

    OpenOptions::new()
        .create_new(true)
        .write(true)
        .append(true)
        .open(path)
        .unwrap();

    // https://github.com/rusqlite/rusqlite
    let conn = Connection::open(path)?;

    conn.execute(
        "CREATE TABLE games (
            id         INTEGER PRIMARY KEY,
            seed       BLOB,
            players    TEXT,
            roles      TEXT,
            complete   INTEGER DEFAULT 0,
            timestamp  TEXT DEFAULT now

        )",
        (), // empty list of parameters.
    )?;

    conn.execute(
        "CREATE TABLE actions (
            id         INTEGER PRIMARY KEY,
            game_id    INTEGER,
            data       TEXT,
            FOREIGN KEY(game_id) REFERENCES game(id)
        )",
        (), // empty list of parameters.
    )?;

    Ok(())
}
