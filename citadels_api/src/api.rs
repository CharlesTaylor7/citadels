use crate::actions::Action;
use crate::notifications::Notifications;
use arcstr::ArcStr;
use poem::web::Data;
use poem::web::Query;
use poem_openapi::ApiResponse;
use poem_openapi::Object;
use poem_openapi::OpenApi;
use poem_openapi::param::{Path, Query};
use poem_openapi::payload::{Json, PlainText};
use sqlx::{Pool, Postgres};

type DB = Pool<Postgres>;
pub struct Api;

#[derive(ApiResponse)]
enum CreateResponse {
    #[oai(status = 201)]
    Created(PlainText<&'static str>, #[oai(header = "location")] String),
}

// #[derive(ApiResponse)]
// enum CreatedGame {
//     #[oai(header = "Location")]
//     location: String,
//     content: PlainText<&'static str>,
// }

#[OpenApi]
impl Api {
    #[oai(path = "/games", method = "post")]
    async fn new_game(&self, body: Json<Game>, db: Data<&DB>) -> CreateResponse {
        let id = sqlx::query!(
            "insert into games (title) values ($1) returning id",
            body.0.title
        )
        .fetch_one(db.0)
        .await
        .unwrap()
        .id;
        CreateResponse::Created(PlainText("success"), format!("/api/games/{}", id))
    }

    #[oai(path = "/games/:id", method = "get")]
    async fn get_game(&self, id: Path<i32>, db: Data<&DB>) -> Json<Game> {
        let title = sqlx::query!("select title from games where id = $1", id.0)
            .fetch_one(db.0)
            .await
            .unwrap()
            .title
            .unwrap_or("".to_string());

        Json(Game { title })
    }
}

#[derive(Object)]
pub struct Game {
    title: String,
}
