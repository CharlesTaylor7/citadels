use super::utils::CreateResponse;
use crate::api::tags::ApiTags;
use crate::{db::DB, errors::RequestError};
use poem::web::Data;
use poem_openapi::{
    Object, OpenApi,
    param::Path,
    payload::{Json, PlainText},
};
pub struct LobbyApi;

#[allow(unused)]
#[OpenApi(tag = "ApiTags::Lobby", prefix_path = "/rooms")]
impl LobbyApi {
    #[oai(path = "/", method = "post")]
    async fn create_room(&self, db: Data<&DB>) -> poem::Result<CreateResponse<Room>> {
        todo!()
    }

    #[oai(path = "/", method = "get")]
    async fn get_room_list(&self, db: Data<&DB>) -> poem::Result<Json<Vec<Room>>> {
        todo!()
    }

    #[oai(path = "/:id", method = "get")]
    async fn get_room(&self, id: Path<i32>, db: Data<&DB>) -> poem::Result<Json<Room>> {
        todo!()
    }

    #[oai(path = "/:id/claim", method = "post")]
    async fn claim_room(&self, db: Data<&DB>) -> poem::Result<Json<Room>> {
        todo!()
    }

    #[oai(path = "/:id/transfer", method = "post")]
    async fn transfer_owner_room(
        &self,
        target: Json<TransferTo>,
        db: Data<&DB>,
    ) -> poem::Result<Json<Room>> {
        todo!()
    }

    #[oai(path = "/:id/join", method = "post")]
    async fn join_room(&self, target: Json<TransferTo>, db: Data<&DB>) -> poem::Result<Json<Room>> {
        todo!()
    }

    #[oai(path = "/:id/leave", method = "post")]
    async fn leave_room(&self, db: Data<&DB>) -> poem::Result<PlainText<String>> {
        let user_id: i32 = todo!();
        sqlx::query!("delete from room_members where player_id = $1", user_id)
            .execute(db.0)
            .await;

        Ok(PlainText("Left room".to_string()))
    }

    #[oai(path = "/:id/rename", method = "post")]
    async fn rename_room(&self, title: Json<RoomTitle>, db: Data<&DB>) -> poem::Result<Json<Room>> {
        todo!()
    }
}

#[derive(Object)]
pub struct RoomTitle {
    title: String,
}

#[allow(non_snake_case)]
#[derive(Object)]
pub struct TransferTo {
    userId: i32,
}

#[derive(Object)]
pub struct Room {}
