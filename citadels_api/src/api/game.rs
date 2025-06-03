use crate::api::tags::ApiTags;
use crate::api::utils::CreateResponse;
use crate::db::DB;
use crate::errors::RequestError;
use poem::web::Data;
use poem_openapi::Object;
use poem_openapi::OpenApi;
use poem_openapi::param::Path;
use poem_openapi::payload::Json;

pub struct GameApi;

#[allow(unused)]
#[OpenApi(tag = "ApiTags::Game", prefix_path = "/games")]
impl GameApi {
    #[oai(path = "/", method = "post")]
    async fn new_game(
        &self,
        body: Json<Game>,
        db: Data<&DB>,
    ) -> poem::Result<CreateResponse<Game>> {
        let id = sqlx::query!("insert into games (state) values ('{}') returning id",)
            .fetch_one(db.0)
            .await
            .map_err(RequestError::Database)?
            .id;
        Ok(CreateResponse::Created(
            Json(Game { id }),
            format!("/api/games/{}", id),
        ))
    }

    #[oai(path = "/:id", method = "get")]
    async fn get_game(&self, id: Path<i32>, db: Data<&DB>) -> poem::Result<Json<Game>> {
        let title = sqlx::query!("select state from games where id = $1", id.0)
            .fetch_one(db.0)
            .await
            .map_err(RequestError::Database)?
            .state
            .to_string();

        Ok(Json(Game { id: 42 }))
    }
}

#[derive(Object)]
pub struct Game {
    id: i32,
}
