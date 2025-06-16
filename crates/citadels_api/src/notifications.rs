use arcstr::ArcStr;
use poem::handler;
use poem::http::StatusCode;
use poem::web::sse::{Event, SSE};
use poem::web::{Data, Json};
use serde::Deserialize;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc::{self, UnboundedSender};
use tokio_stream::wrappers::UnboundedReceiverStream;

use crate::errors::RequestResult;

pub type NotificationHandle = UnboundedSender<Event>;

#[derive(Clone)]
pub struct Notifications(Arc<Mutex<HashMap<RoomId, HashMap<PlayerId, NotificationHandle>>>>);

impl Notifications {
    pub fn notify_room(&self, room_id: RoomId, message: ArcStr) -> RequestResult<()> {
        let event = Event::message(message.to_string());
        // TODO: decide on a comprehensive error handling approach
        self.0
            .lock()
            .unwrap()
            .get_mut(&room_id)
            .unwrap()
            .values_mut()
            .for_each(|handle| handle.send(event.clone()).unwrap());
        Ok(())
    }
    pub fn notify_player(
        &self,
        room_id: RoomId,
        player_id: PlayerId,
        message: ArcStr,
    ) -> RequestResult<()> {
        let event = Event::message(message.to_string());
        self.0
            .lock()
            .unwrap()
            .get_mut(&room_id)
            .unwrap()
            .get_mut(&player_id)
            .unwrap()
            .send(event)
            .unwrap();
        Ok(())
    }
}
impl Default for Notifications {
    fn default() -> Self {
        Notifications(Arc::new(Mutex::new(HashMap::new())))
    }
}

#[handler]
pub async fn sse_handler(
    request: Json<SubscribeRequest>,
    notifications: Data<&Notifications>,
) -> poem::Result<SSE> {
    let mut handles = match notifications.0.0.lock() {
        Ok(handles) => handles,
        Err(_) => return Err(poem::Error::from_status(StatusCode::INTERNAL_SERVER_ERROR)),
    };

    let (sender, receiver) = mpsc::unbounded_channel::<Event>();

    handles
        .entry(request.roomId.clone())
        .or_insert_with(HashMap::new)
        .insert(request.playerId.clone(), sender);

    Ok(SSE::new(UnboundedReceiverStream::new(receiver)))
}

#[derive(Deserialize, PartialEq, Eq, Hash, Clone)]
pub struct RoomId(ArcStr);

#[derive(Deserialize, PartialEq, Eq, Hash, Clone)]
pub struct PlayerId(ArcStr);

#[allow(non_snake_case)]
#[derive(Deserialize, Clone)]
pub struct SubscribeRequest {
    roomId: RoomId,
    playerId: PlayerId,
}
