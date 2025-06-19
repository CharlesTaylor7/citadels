use super::utils::CreateResponse;
use crate::api::tags::ApiTags;
use crate::db::DB;

use poem::session::Session;
use poem::web::Data;
use poem_openapi::{
    Object, OpenApi,
    param::Path,
    payload::{Json, PlainText},
};
use serde::Deserialize;
use sqlx::types::Uuid;

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
        let rooms = sqlx::query!(
            r#"SELECT 
                rooms.id, 
                rooms.name, 
                JSON_AGG(
                    JSON_BUILD_OBJECT(
                        'id', room_members.user_id,
                        'username', users.username,
                        'owner', room_members.owner
                    )
                ) as players 
                FROM rooms 
                LEFT JOIN room_members ON room_members.room_id = rooms.id 
                LEFT JOIN users ON room_members.user_id = users.id 
                GROUP by rooms.id"#
        )
        .fetch_all(db.0)
        .await
        .unwrap()
        .iter()
        .map(|raw| Room {
            id: raw.id,
            name: raw.name.to_string(),
            players: serde_json::from_value(raw.players.as_ref().unwrap().clone())
                .unwrap_or(vec![]),
        })
        .collect::<Vec<_>>();

        Ok(Json(rooms))
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
        let session_id: Uuid = session.get("session_id").unwrap();
        sqlx::query!(
            "update room_members rm set owner = true from sessions s where rm.user_id = s.user_id and s.id = $1",
            session_id
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
        let session_id: Uuid = session.get("session_id").unwrap();
        let mut tx = db.0.begin().await.unwrap();

        sqlx::query!(
            "update room_members rm set owner = false from sessions s where rm.user_id = s.user_id and s.id = $1",
            session_id
        )
        .execute(&mut *tx)
        .await
        .unwrap();

        sqlx::query!(
            "update room_members set owner = true where user_id = $1",
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
        let session_id: Uuid = session.get("session_id").unwrap();
        sqlx::query!(
            "insert into room_members(room_id, user_id) select $1, u.id from sessions s join users u on u.id = s.user_id where s.id = $2",
            id.0,
            session_id
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
        let session_id: Uuid = session.get("session_id").unwrap();
        sqlx::query!("delete from room_members rm using sessions s where s.user_id = rm.user_id and s.id = $1", session_id)
            .execute(db.0)
            .await;

        Ok(PlainText("Left room".to_string()))
    }

    #[oai(path = "/:id/rename", method = "post")]
    async fn rename_room(
        &self,
        id: Path<i32>,
        body: Json<RoomTitle>,
        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<PlainText<String>> {
        let user_id: Uuid = session.get("user_id").unwrap();
        sqlx::query!("update rooms set name = $1 where id = $2", body.title, id.0)
            .execute(db.0)
            .await;

        Ok(PlainText("Renamed room".to_string()))
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
pub struct Room {
    id: i32,
    name: String,
    players: Vec<Player>,
}

#[derive(Object, Deserialize)]
pub struct Player {
    id: Uuid,
    username: String,
    owner: bool,
}
