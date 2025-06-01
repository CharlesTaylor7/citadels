use poem::{
    Endpoint, EndpointExt, FromRequest, IntoResponse, Middleware, Request, Response, Result, Route,
    Server, get, handler, listener::TcpListener, session::Session, web::Data,
};
use sqlx::Pool;

use crate::db::DB;

pub struct PlayerSessions;

impl<E: Endpoint> Middleware<E> for PlayerSessions {
    type Output = PlayerSessionsImpl<E>;

    fn transform(&self, endpoint: E) -> Self::Output {
        PlayerSessionsImpl(endpoint)
    }
}

pub struct PlayerSessionsImpl<E>(E);

impl<E: Endpoint> Endpoint for PlayerSessionsImpl<E> {
    type Output = E::Output;

    async fn call(&self, mut req: Request) -> Result<Self::Output> {
        let session = <&Session>::from_request_without_body(&req).await.unwrap();
        // let user_id: Option<i32> = session.get("user_id");
        // let user_id = match user_id {
        //     Some(user_id) => {
        //         println!("Request from existing user id: {}", user_id);
        //         user_id
        //     }
        //     None => {
        //         let db = <Data<&DB>>::from_request_without_body(&req).await.unwrap();
        //         let user_id = sqlx::query!(
        //             "insert into users(username, guest) values(NULL, true) returning id"
        //         )
        //         .fetch_one(db.0)
        //         .await
        //         .unwrap()
        //         .id;
        //         session.set("user_id", user_id);
        //
        //         println!("Created new guest user with id: {}", user_id);
        //         user_id
        //     }
        // };
        //
        // let game_id = session.get("game_id");
        // let game_id = match game_id {
        //     Some(game_id) => Some(game_id),
        //     None => {
        //         let db = <Data<&DB>>::from_request_without_body(&req).await.unwrap();
        //         let id: Option<i32>  = sqlx::query!("select rooms.game_id from room_members join rooms on rooms.id = room_members.room_id where room_members.player_id = $1", user_id).fetch_optional(db.0).await.unwrap().and_then(|row| row.game_id);
        //         id
        //     }
        // };
        //
        // req.extensions_mut()
        //     .insert(PlayerSession { user_id, game_id });
        //
        self.0.call(req).await
    }
}

#[derive(Clone)]
pub struct PlayerSession {
    user_id: i32,
    game_id: Option<i32>,
}
