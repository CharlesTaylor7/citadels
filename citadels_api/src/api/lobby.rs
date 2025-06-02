use crate::api::tags::ApiTags;
use poem::web::Data;
use poem_openapi::{OpenApi, param::Path, payload::PlainText};

use crate::{db::DB, errors::RequestError};

pub struct LobbyApi;

#[allow(unused)]
#[OpenApi(tag = "ApiTags::Lobby", prefix_path = "/rooms")]
impl LobbyApi {
    #[oai(path = "/:id", method = "get")]
    async fn get_room(&self, id: Path<i32>, db: Data<&DB>) -> poem::Result<PlainText<String>> {
        let title = sqlx::query!("select state from games where id = $1", id.0)
            .fetch_one(db.0)
            .await
            .map_err(RequestError::Database)?
            .state
            .to_string();

        Ok(PlainText("Hello".to_string()))
    }
}
