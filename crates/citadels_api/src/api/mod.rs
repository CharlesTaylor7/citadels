use auth::AuthApi;
use game::GameApi;
use lobby::LobbyApi;
use poem_openapi::OpenApiService;

pub mod auth;
pub mod game;
pub mod lobby;
pub mod tags;
pub mod utils;

pub const PORT: u32 = 8000;

pub type Api = (AuthApi, LobbyApi, GameApi);

pub fn service() -> OpenApiService<Api, ()> {
    OpenApiService::new((AuthApi, LobbyApi, GameApi), "Citadels API", "1.0")
        .server(format!("http://localhost:{}/api", PORT))
}
