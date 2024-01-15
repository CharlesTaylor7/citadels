use crate::server::state::AppState;
use axum::extract::ws::{Message, WebSocket};
use axum::extract::State;
use axum::response::{ErrorResponse, Html};
use axum::Error;
use futures::stream::StreamExt;
use std::collections::hash_map::HashMap;
use tokio;
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;

type WebSocketSink = mpsc::UnboundedSender<Result<Message, Error>>;

#[derive(Default)]
pub struct Connections(HashMap<String, WebSocketSink>);

impl Connections {
    pub fn broadcast(&mut self, html: Html<String>) {
        self.0.values_mut().for_each(|ws| {
            let _ = ws.send(Ok(Message::Text(html.0.clone())));
        });
    }

    pub fn broadcast_each<'a, F>(&'a mut self, to_html: F)
    where
        F: Fn(&'a str) -> Result<Html<String>, ErrorResponse>,
    {
        for (key, ws) in self.0.iter_mut() {
            match to_html(key) {
                Ok(html) => {
                    let _ = ws.send(Ok(Message::Text(html.0.clone())));
                }
                Err(e) => log::debug!("{:#?}", e),
            }
        }
    }
}

pub async fn handle_socket(state: State<AppState>, player_id: String, socket: WebSocket) {
    let (ws_sender, mut ws_recv) = socket.split();
    let (chan_sender, chan_recv) = mpsc::unbounded_channel();
    tokio::spawn(UnboundedReceiverStream::new(chan_recv).forward(ws_sender));

    state
        .connections
        .lock()
        .unwrap()
        .0
        .insert(player_id, chan_sender);

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

        // axum automatically replies to ping
        Message::Ping(_) => {
            log::trace!("WS - Ping")
        }
        Message::Pong(_) => {
            log::trace!("WS - Pong")
        }
    }
    Ok(())
}
