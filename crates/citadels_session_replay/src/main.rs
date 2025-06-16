use citadels::{
    game::Game,
    lobby::{GameConfig, Player},
    random::Seed,
};
use serde::Deserialize;
use sqlx::{Pool, Postgres};
use sqlx_postgres::PgPoolOptions;
use std::borrow::Borrow;

#[tokio::main]
async fn main() {
    dotenvy::dotenv().expect(".env not found");
    let database_url = std::env::var("PROD_DATABASE_URL").expect("PROD_DATABASE_URL must be set");
    let db = PgPoolOptions::default()
        .max_connections(1)
        .connect(&database_url)
        .await
        .unwrap();

    replay(&db).await;
}

async fn replay(db: &Pool<Postgres>) {
    let actions =
        sqlx::query!("select action, player_name from logs where game_id = 1 order by created_at")
            .fetch_all(db)
            .await
            .unwrap();

    let start: GameStart =
        serde_json::from_value(actions[0].action.as_ref().unwrap().clone()).unwrap();
    let mut game = Game::start(
        citadels::lobby::Lobby {
            players: start.players,
            config: start.config,
        },
        start.seed,
    )
    .unwrap();

    println!("{:#?}", game);
    for row in actions.iter().skip(1) {
        let action: citadels::actions::Action =
            serde_json::from_value(row.action.as_ref().unwrap().clone()).unwrap();

        println!("{:#?}", &action);
        let player_name: &str = row.player_name.as_ref().unwrap();
        let p = game
            .players
            .iter()
            .find(|p| Borrow::<str>::borrow(&p.name.0) == player_name)
            .unwrap();
        let id = p.id.clone();
        game.perform(action, &id);
        println!("{:#?}", game);
    }
}

#[derive(Deserialize)]
pub struct GameStart {
    seed: Seed,
    config: GameConfig,
    players: Vec<Player>,
}
