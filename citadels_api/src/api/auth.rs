use crate::api::tags::ApiTags;
use poem_openapi::{
    Object, OpenApi,
    payload::{Json, PlainText},
};
pub struct AuthApi;

#[derive(Object)]
pub struct UserSignup {
    email: String,
    username: String,
    password: String,
}

#[derive(Object)]
pub struct UserLogin {
    email: String,
    username: String,
    password: String,
}

#[derive(Object)]
pub struct User {
    id: i32,
    email: String,
    username: String,
}

#[OpenApi(tag = "ApiTags::Auth", prefix_path = "/auth")]
impl AuthApi {
    #[oai(path = "/signup", method = "post")]

    async fn signup(&self) -> poem::Result<PlainText<String>> {
        todo!()
    }
    #[oai(path = "/login", method = "post")]
    async fn login(&self) -> poem::Result<PlainText<String>> {
        todo!()
    }

    #[oai(path = "/logout", method = "post")]
    async fn logout(&self) -> poem::Result<PlainText<String>> {
        todo!()
    }

    #[oai(path = "/me", method = "get")]
    async fn me(&self) -> poem::Result<Json<User>> {
        todo!()
    }
}
