use citadels::actions::{ActionTag, Tag};
use citadels::roles::RoleName;
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

    replay(&db, 1).await;
}

async fn replay(db: &Pool<Postgres>, game_id: i32) {
    let actions = sqlx::query!(
        "select action, player_name from logs where game_id = $1 order by created_at",
        game_id
    )
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

    for row in actions.iter().skip(1) {
        let action: citadels::actions::Action =
            serde_json::from_value(row.action.as_ref().unwrap().clone()).unwrap();

        let player_name: &str = row.player_name.as_ref().unwrap();

        println!("{}: {:#?}", player_name, &action);
        let p = game
            .players
            .iter()
            .find(|p| Borrow::<str>::borrow(&p.name.0) == player_name)
            .unwrap();
        let id = p.id.clone();

        let tag = action.tag();
        if tag == ActionTag::TakeCrown {
            println!("{:#?}", game.characters.get(RoleName::Patrician));
        }
        game.perform(action, &id).unwrap();

        if tag == ActionTag::TakeCrown {
            println!("{:#?}", game.characters.get(RoleName::Patrician));
        }
    }
    //
    // sqlx::query!(
    //     "update games set state = $1 where id = 1",
    //     serde_json::to_value(game).unwrap()
    // )
    // .execute(db)
    // .await
    // .unwrap();
}

#[derive(Deserialize)]
pub struct GameStart {
    seed: Seed,
    config: GameConfig,
    players: Vec<Player>,
}
