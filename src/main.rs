use citadels::lobby::Lobby;
use std::sync::{Arc, Mutex};

use axum::{
    extract::FromRef,
    routing::{get, post},
    Router,
};
use load_dotenv::load_dotenv;
use minijinja;
use tokio;
use tower_http::services::ServeDir;

use axum_extra::extract::cookie;

// TODO:
// - [x] run on 8080
// - [x] serve public/index.css
// - [x] load jinja templates
// - [x] cookies
// - routes
// - [x] /
// - [x] /register
// - [ ] /ws
// - [ ] /start

#[derive(Clone)]
pub struct Context {
    pub cookie_signing_key: cookie::Key,
    pub jinja_env: minijinja::Environment<'static>,
    pub lobby: Arc<Mutex<Lobby>>,
}

impl FromRef<Context> for cookie::Key {
    fn from_ref(context: &Context) -> Self {
        context.cookie_signing_key.clone()
    }
}

#[tokio::main]
async fn main() {
    load_dotenv!();
    let port = "0.0.0.0:8080";
    let listener = tokio::net::TcpListener::bind(port).await.unwrap();

    println!("\nListening on port: {}", port);
    axum::serve(listener, router()).await.unwrap();
}

fn router() -> Router {
    let mut env = minijinja::Environment::new();
    use template_fragments::*;
    for (path, template) in [(
        "lobby.html",
        std::fs::read_to_string("./templates/lobby.html").unwrap(),
    )] {
        for (fragment_name, template_fragment) in split_templates(&template).unwrap() {
            env.add_template_owned(join_path(path, &fragment_name), template_fragment)
                .unwrap();
        }
    }
    let context = Context {
        cookie_signing_key: cookie::Key::from(env!("COOKIE_SIGNING_KEY").as_bytes()),
        jinja_env: env,
        lobby: Arc::new(Mutex::new(Lobby::default())),
    };

    Router::new()
        .route("/", get(handlers::index))
        .route("/register", post(handlers::register))
        .route("/ws", get(handlers::ws))
        .nest_service("/public", ServeDir::new("public"))
        .with_state(context)
}

fn template<'a, S: serde::Serialize>(
    env: &minijinja::Environment,
    template: &'a str,
    ctx: S,
) -> axum::response::Html<String> {
    use axum::response::Html;

    Html(env.get_template(template).unwrap().render(ctx).unwrap())
}

mod handlers {

    use axum::{
        extract::ws::{Message, WebSocket, WebSocketUpgrade},
        response::IntoResponse,
    };
    use axum_extra::{headers::UserAgent, TypedHeader};

    use std::borrow::Cow;
    use std::ops::ControlFlow;
    use std::{net::SocketAddr, path::PathBuf};
    use tower_http::trace::{DefaultMakeSpan, TraceLayer};

    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

    //allows to extract the IP of connecting user
    use axum::extract::connect_info::ConnectInfo;
    use axum::extract::ws::CloseFrame;

    //allows to split the websocket stream into separate TX and RX branches
    use futures::{sink::SinkExt, stream::StreamExt};

    use std::collections::hash_map::*;

    use crate::Context;
    use axum::{extract::State, response::IntoResponse};
    use axum_extra::extract::{cookie::Cookie, PrivateCookieJar};
    use citadels::lobby::*;
    use minijinja::context;
    use uuid::Uuid;

    pub async fn index(
        state: State<Context>,
        cookies: PrivateCookieJar,
    ) -> impl axum::response::IntoResponse {
        let username = cookies
            .get("username")
            .map_or("".to_owned(), |c| c.value().to_owned());

        let player_id = cookies.get("playerId").map(|c| c.value().to_owned());

        let players: Vec<Player> = {
            let lobby = state.lobby.lock().unwrap();
            lobby
                .seating
                .iter()
                .filter_map(|id| lobby.players.get(id))
                .cloned()
                .collect()
        };

        (
            cookies.add(Cookie::new(
                "playerId",
                player_id.unwrap_or_else(|| Uuid::new_v4().to_string()),
            )),
            crate::template(
                &state.jinja_env,
                "lobby.html",
                context!(
                    username => username,
                    players => players,
                ),
            ),
        )
    }
    use serde::Deserialize;

    #[derive(Deserialize)]
    pub struct Register {
        username: String,
    }

    pub async fn register(
        state: State<Context>,
        cookies: PrivateCookieJar,
        args: axum::Form<Register>,
    ) -> impl IntoResponse {
        let cookie = cookies.get("playerId").unwrap();
        let player_id = cookie.value();
        let players: Vec<Player> = {
            let mut lobby = state.lobby.lock().unwrap();
            match lobby.players.entry(player_id.to_owned()) {
                Entry::Occupied(e) => {
                    e.into_mut().name = args.username.clone();
                }
                Entry::Vacant(e) => {
                    e.insert(Player {
                        id: player_id.to_owned(),
                        name: args.username.to_owned(),
                    });
                    lobby.seating.push(player_id.to_owned())
                }
            }
            lobby
                .seating
                .iter()
                .filter_map(|id| lobby.players.get(id))
                .cloned()
                .collect()
        };

        (
            cookies.add(Cookie::new("username", args.username.clone())),
            crate::template(
                &state.jinja_env,
                "lobby.html#players",
                context!(
                    players => players,
                ),
            ),
        )
    }

    /// The handler for the HTTP request (this gets called when the HTTP GET lands at the start
    /// of websocket negotiation). After this completes, the actual switching from HTTP to
    /// websocket protocol will occur.
    /// This is the last point where we can extract TCP/IP metadata such as IP address of the client
    /// as well as things from HTTP headers such as user-agent of the browser etc.
    pub async fn ws(
        ws: WebSocketUpgrade,
        user_agent: Option<TypedHeader<UserAgent>>,
        ConnectInfo(addr): ConnectInfo<SocketAddr>,
    ) -> impl IntoResponse {
        let user_agent = if let Some(TypedHeader(user_agent)) = user_agent {
            user_agent.to_string()
        } else {
            String::from("Unknown browser")
        };
        println!("`{user_agent}` at {addr} connected.");
        // finalize the upgrade process by returning upgrade callback.
        // we can customize the callback by sending additional info such as address.
        ws.on_upgrade(move |socket| crate::ws::handle_socket(socket, addr))
    }
}

pub mod ws {
    use axum::{
        extract::ws::{Message, WebSocket, WebSocketUpgrade},
        response::IntoResponse,
    };
    use axum_extra::{headers::UserAgent, TypedHeader};

    use std::borrow::Cow;
    use std::ops::ControlFlow;
    use std::{net::SocketAddr, path::PathBuf};
    use tower_http::trace::{DefaultMakeSpan, TraceLayer};

    use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

    //allows to extract the IP of connecting user
    use axum::extract::connect_info::ConnectInfo;
    use axum::extract::ws::CloseFrame;

    //allows to split the websocket stream into separate TX and RX branches
    use futures::{sink::SinkExt, stream::StreamExt};

    /// Actual websocket statemachine (one will be spawned per connection)
    pub async fn handle_socket(mut socket: WebSocket, who: SocketAddr) {
        //send a ping (unsupported by some browsers) just to kick things off and get a response
        if socket.send(Message::Ping(vec![1, 2, 3])).await.is_ok() {
            println!("Pinged {who}...");
        } else {
            println!("Could not send ping {who}!");
            // no Error here since the only thing we can do is to close the connection.
            // If we can not send messages, there is no way to salvage the statemachine anyway.
            return;
        }

        // receive single message from a client (we can either receive or send with socket).
        // this will likely be the Pong for our Ping or a hello message from client.
        // waiting for message from a client will block this task, but will not block other client's
        // connections.
        if let Some(msg) = socket.recv().await {
            if let Ok(msg) = msg {
                if process_message(msg, who).is_break() {
                    return;
                }
            } else {
                println!("client {who} abruptly disconnected");
                return;
            }
        }

        // Since each client gets individual statemachine, we can pause handling
        // when necessary to wait for some external event (in this case illustrated by sleeping).
        // Waiting for this client to finish getting its greetings does not prevent other clients from;
        // connecting to server and receiving their greetings.
        for i in 1..5 {
            if socket
                .send(Message::Text(format!("Hi {i} times!")))
                .await
                .is_err()
            {
                println!("client {who} abruptly disconnected");
                return;
            }
            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
        }

        // By splitting socket we can send and receive at the same time. In this example we will send
        // unsolicited messages to client based on some sort of server's internal event (i.e .timer).
        let (mut sender, mut receiver) = socket.split();

        // Spawn a task that will push several messages to the client (does not matter what client does)
        let mut send_task = tokio::spawn(async move {
            let n_msg = 20;
            for i in 0..n_msg {
                // In case of any websocket error, we exit.
                if sender
                    .send(Message::Text(format!("Server message {i} ...")))
                    .await
                    .is_err()
                {
                    return i;
                }

                tokio::time::sleep(std::time::Duration::from_millis(300)).await;
            }

            println!("Sending close to {who}...");
            if let Err(e) = sender
                .send(Message::Close(Some(CloseFrame {
                    code: axum::extract::ws::close_code::NORMAL,
                    reason: Cow::from("Goodbye"),
                })))
                .await
            {
                println!("Could not send Close due to {e}, probably it is ok?");
            }
            n_msg
        });

        // This second task will receive messages from client and print them on server console
        let mut recv_task = tokio::spawn(async move {
            let mut cnt = 0;
            while let Some(Ok(msg)) = receiver.next().await {
                cnt += 1;
                // print message and break if instructed to do so
                if process_message(msg, who).is_break() {
                    break;
                }
            }
            cnt
        });

        // If any one of the tasks exit, abort the other.
        tokio::select! {
            rv_a = (&mut send_task) => {
                match rv_a {
                    Ok(a) => println!("{a} messages sent to {who}"),
                    Err(a) => println!("Error sending messages {a:?}")
                }
                recv_task.abort();
            },
            rv_b = (&mut recv_task) => {
                match rv_b {
                    Ok(b) => println!("Received {b} messages"),
                    Err(b) => println!("Error receiving messages {b:?}")
                }
                send_task.abort();
            }
        }

        // returning from the handler closes the websocket connection
        println!("Websocket context {who} destroyed");
    }

    /// helper to print contents of messages to stdout. Has special treatment for Close.
    fn process_message(msg: Message, who: SocketAddr) -> ControlFlow<(), ()> {
        match msg {
            Message::Text(t) => {
                println!(">>> {who} sent str: {t:?}");
            }
            Message::Binary(d) => {
                println!(">>> {} sent {} bytes: {:?}", who, d.len(), d);
            }
            Message::Close(c) => {
                if let Some(cf) = c {
                    println!(
                        ">>> {} sent close with code {} and reason `{}`",
                        who, cf.code, cf.reason
                    );
                } else {
                    println!(">>> {who} somehow sent close message without CloseFrame");
                }
                return ControlFlow::Break(());
            }

            // axum's automatically replies to ping
            Message::Ping(v) => {}
            Message::Pong(v) => {}
        }
        ControlFlow::Continue(())
    }
}
