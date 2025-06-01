use poem_openapi::Tags;

#[derive(Tags)]
pub enum ApiTags {
    Lobby,
    Game,
    Auth,
}
