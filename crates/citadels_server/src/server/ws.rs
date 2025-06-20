use futures::{StreamExt, TryStream};
use poem::web::{
    websocket::{Message, WebSocketStream},
    Html,
};
use std::collections::hash_map::HashMap;
use std::sync::Arc;
use tokio;
use tokio::sync::mpsc;
use tokio::sync::Mutex;
use tokio_stream::wrappers::UnboundedReceiverStream;

type WebSocketSink = mpsc::UnboundedSender<Result<Message, <WebSocketStream as TryStream>::Error>>;

/// Handles web socket connections for sending html over the wire
#[derive(Default, Clone)]
pub struct WsHtmxHandles(Arc<Mutex<HashMap<String, WebSocketSink>>>);

impl WsHtmxHandles {
    pub async fn broadcast(&self, html: Html<String>) {
        self.0.lock().await.values_mut().for_each(|ws| {
            let _ = ws.send(Ok(Message::Text(html.0.clone())));
        });
    }

    pub async fn broadcast_each<F>(&self, to_html: F)
    where
        F: Fn(&str) -> Html<String>,
    {
        let mut handle = self.0.lock().await;
        for (key, ws) in handle.iter_mut() {
            let html = to_html(key).0;
            let _ = ws.send(Ok(Message::Text(html)));
        }
    }
}

pub async fn handle_socket(conn: WsHtmxHandles, player_id: String, ws: WebSocketStream) {
    let (ws_sender, mut ws_recv) = ws.split();
    let (chan_sender, chan_recv) = mpsc::unbounded_channel();
    // why was forward used here?
    // why does forward impose a result type on all my sent messages
    conn.0.lock().await.insert(player_id, chan_sender);
    log::info!("WS - connected");

    tokio::spawn(UnboundedReceiverStream::new(chan_recv).forward(ws_sender));
    tokio::spawn(async move {
        while let Some(Ok(msg)) = ws_recv.next().await {
            if process_message(msg).is_err() {
                break;
            }
        }
    });
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
