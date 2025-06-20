use citadels_api::api;
use citadels_api::notifications::{Notifications, sse_handler};
use citadels_server::server::routes::htmx_routes;
use citadels_server::server::state::AppState;
use poem::endpoint::{EndpointExt, StaticFilesEndpoint};
use poem::listener::TcpListener;
use poem::middleware::{AddData, Tracing};
use poem::session::{CookieConfig, CookieSession};
use poem::web::cookie::SameSite;
use poem::{Server, post};
use sqlx_postgres::PgPoolOptions;

#[tokio::main]
async fn main() {
    #[cfg(feature = "dotenv")]
    dotenvy::dotenv().expect(".env not found");
    color_eyre::install().expect("color-eyre could not be installed");
    let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    let context = AppState::default();
    let pool = PgPoolOptions::new()
        .max_connections(50)
        .connect(&database_url)
        .await
        .expect("Could not connect to database");

    let api_service = api::service();
    let ui = api_service.swagger_ui();

    let app = htmx_routes()
        .at("/sse", post(sse_handler))
        .nest("/api", api_service)
        .nest("/docs", ui)
        // // static assets
        .nest("/public", StaticFilesEndpoint::new("public"))
        // spa
        // .at(
        //     "/*path",
        //     StaticFilesEndpoint::new("public/spa")
        //         .no_cache_index()
        //         .index_file("index.html")
        //         .fallback_to_index(),
        // )
        .with(AddData::new(context))
        .with(AddData::new(Notifications::default()))
        .with(AddData::new(pool))
        .with(CookieSession::new(
            CookieConfig::default()
                .secure(true)
                .same_site(SameSite::Lax),
        ))
        .with(Tracing);

    let _ = Server::new(TcpListener::bind(format!("0.0.0.0:{}", api::PORT)))
        .run(app)
        .await;
}
