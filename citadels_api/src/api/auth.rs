use crate::api::tags::ApiTags;
use poem_openapi::{OpenApi, payload::PlainText};
pub struct AuthApi;

#[OpenApi(tag = "ApiTags::Auth", prefix_path = "/auth")]
impl AuthApi {
    #[oai(path = "/login", method = "post")]
    async fn login(&self) -> poem::Result<PlainText<String>> {
        Ok(PlainText("TODO".to_string()))
    }

    #[oai(path = "/logout", method = "post")]
    async fn logout(&self) -> poem::Result<PlainText<String>> {
        Ok(PlainText("TODO".to_string()))
    }

    #[oai(path = "/signup", method = "post")]
    async fn signup(&self) -> poem::Result<PlainText<String>> {
        Ok(PlainText("TODO".to_string()))
    }

    #[oai(path = "/me", method = "get")]
    async fn me(&self) -> poem::Result<PlainText<String>> {
        Ok(PlainText("TODO".to_string()))
    }
}
