use futures::{stream::SplitSink, SinkExt, StreamExt};
use poem::web::{
    websocket::{Message, WebSocketStream},
    Html,
};
use std::collections::hash_map::HashMap;

type WebSocketSink = SplitSink<WebSocketStream, Message>;

#[derive(Default)]
pub struct Connections(HashMap<String, WebSocketSink>);

impl Connections {
    pub fn broadcast(&mut self, html: Html<String>) {
        self.0.values_mut().for_each(|ws| {
            let _ = ws.send((Message::Text(html.0.clone())));
        });
    }

    pub fn broadcast_each<'a, F>(&'a mut self, to_html: F)
    where
        F: Fn(&'a str) -> Html<String>,
    {
        for (key, ws) in self.0.iter_mut() {
            let _ = ws.send((Message::Text(to_html(key).0.clone())));
        }
    }
}

pub async fn handle_socket(conn: &mut Connections, player_id: String, ws: WebSocketStream) {
    let (ws_sender, mut ws_recv) = ws.split();
    conn.0.insert(player_id, ws_sender);
    log::info!("WS - connected");

    while let Some(Ok(msg)) = ws_recv.next().await {
        if process_message(msg).is_err() {
            break;
        }
    }
}

fn process_message(msg: Message) -> Result<(), ()> {
    match msg {
        Message::Text(t) => {
            log::debug!("WS - client sent str: {t:?}");
        }
        Message::Binary(d) => {
            log::debug!("WS - client sent {} bytes: {:?}", d.len(), d);
        }
        Message::Close(_) => {
            log::debug!("WS - closed connection");
            return Err(());
        }

        // automatically replies to ping
        Message::Ping(_) => {
            log::trace!("WS - Ping")
        }
        Message::Pong(_) => {
            log::trace!("WS - Pong")
        }
    }
    Ok(())
}
