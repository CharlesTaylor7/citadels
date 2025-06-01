use citadels_api::api::Api;
use citadels_api::middleware::sessions::PlayerSessions;
use citadels_api::notifications::{Notifications, sse_handler};
use poem::endpoint::{EndpointExt, StaticFilesEndpoint};
use poem::listener::TcpListener;
use poem::middleware::{AddData, Tracing};
use poem::session::{CookieConfig, CookieSession};
use poem::web::cookie::SameSite;
use poem::{Route, Server, post};
use poem_openapi::OpenApiService;
use sqlx_postgres::PgPoolOptions;

#[tokio::main]
async fn main() {
    dotenvy::dotenv().expect(".env not found");

    color_eyre::install().expect("color-eyre could not be installed");
    let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    let pool = PgPoolOptions::new()
        .max_connections(50)
        .connect(&database_url)
        .await
        .expect("Could not connect to database");

    let api_service =
        OpenApiService::new(Api, "Citadels API", "1.0").server("http://localhost:3000/api");
    let ui = api_service.swagger_ui();
    let app = Route::new()
        .at("/sse", post(sse_handler))
        .nest("/api", api_service)
        .nest("/docs", ui)
        .nest("/static", StaticFilesEndpoint::new("../public"))
        .with(PlayerSessions)
        .with(AddData::new(Notifications::default()))
        .with(AddData::new(pool))
        .with(CookieSession::new(
            CookieConfig::default()
                .secure(true)
                .same_site(SameSite::Lax),
        ))
        .with(Tracing);

    let _ = Server::new(TcpListener::bind("127.0.0.1:3000"))
        .run(app)
        .await;
}
