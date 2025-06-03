use super::utils::CreateResponse;
use crate::api::tags::ApiTags;
use crate::{db::DB, errors::RequestError};

use poem::session::Session;
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
    async fn create_room(
        &self,
        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<CreateResponse<Room>> {
        todo!()
    }

    #[oai(path = "/", method = "get")]
    async fn get_room_list(
        &self,

        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<Json<Vec<Room>>> {
        todo!()
    }

    #[oai(path = "/:id", method = "get")]
    async fn get_room(&self, id: Path<i32>, db: Data<&DB>) -> poem::Result<Json<Room>> {
        todo!()
    }

    #[oai(path = "/:id/claim", method = "post")]
    async fn claim_room(
        &self,
        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<PlainText<String>> {
        let user_id: i32 = session.get("user_id").unwrap();
        sqlx::query!(
            "update room_members set owner = true where player_id = $1",
            user_id
        )
        .execute(db.0)
        .await
        .unwrap();

        Ok(PlainText("claimed room".to_string()))
    }

    #[oai(path = "/:id/transfer", method = "post")]
    async fn transfer_owner_room(
        &self,
        id: Path<i32>,
        target: Json<TransferTo>,

        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<PlainText<String>> {
        let user_id: i32 = session.get("user_id").unwrap();
        let mut tx = db.0.begin().await.unwrap();

        sqlx::query!(
            "update room_members set owner = false where player_id = $1",
            user_id
        )
        .execute(&mut *tx)
        .await
        .unwrap();

        sqlx::query!(
            "update room_members set owner = true where player_id = $1",
            target.userId
        )
        .execute(&mut *tx)
        .await
        .unwrap();

        tx.commit().await.unwrap();

        Ok(PlainText("transfered room".to_string()))
    }

    #[oai(path = "/:id/join", method = "post")]
    async fn join_room(
        &self,
        id: Path<i32>,
        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<PlainText<String>> {
        let user_id: i32 = session.get("user_id").unwrap();
        sqlx::query!(
            "insert into room_members(room_id, player_id) values($1, $2)",
            id.0,
            user_id
        )
        .execute(db.0)
        .await
        .unwrap();

        Ok(PlainText("joined room".to_string()))
    }

    #[oai(path = "/:id/leave", method = "post")]
    async fn leave_room(
        &self,
        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<PlainText<String>> {
        let user_id: i32 = session.get("user_id").unwrap();
        sqlx::query!("delete from room_members where player_id = $1", user_id)
            .execute(db.0)
            .await;

        Ok(PlainText("Left room".to_string()))
    }

    #[oai(path = "/:id/rename", method = "post")]
    async fn rename_room(
        &self,
        title: Json<RoomTitle>,
        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<Json<Room>> {
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
