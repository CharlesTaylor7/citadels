use crate::{api::tags::ApiTags, db::DB};
use argon2::{
    Argon2,
    password_hash::{PasswordHash, PasswordHasher, PasswordVerifier, SaltString, rand_core::OsRng},
};
use poem::{
    session::Session,
    web::{Data, Query},
};
use poem_openapi::{
    Object, OpenApi,
    payload::{Json, PlainText},
};
use serde::Deserialize;

pub struct AuthApi;

#[OpenApi(tag = "ApiTags::Auth", prefix_path = "/auth")]
impl AuthApi {
    #[oai(path = "/signup", method = "post")]

    async fn signup(
        &self,
        body: Json<UserSignup>,
        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<PlainText<String>> {
        let salt = SaltString::generate(&mut OsRng);

        // Argon2 with default params (Argon2id v19)
        let argon2 = Argon2::default();

        // Hash password to PHC string ($argon2id$v=19$...)
        let password_hash = argon2
            .hash_password(body.password.as_bytes(), &salt)
            .unwrap()
            .to_string();

        let user_id = sqlx::query!(
            "insert into users (email, username, hashed_password) values ($1, $2, $3) returning id",
            body.email,
            body.username,
            password_hash
        )
        .fetch_one(db.0)
        .await
        .unwrap()
        .id;
        session.set("user_id", user_id);

        Ok(PlainText("Success".to_string()))
    }

    #[oai(path = "/login", method = "post")]
    async fn login(
        &self,
        body: Json<UserLogin>,
        session: &Session,
        db: Data<&DB>,
    ) -> poem::Result<PlainText<String>> {
        let user = sqlx::query!(
            "select id, hashed_password from users where users.username = $1 or users.email = $1",
            body.login
        )
        .fetch_one(db.0)
        .await
        .unwrap();
        let parsed_hash = PasswordHash::new(&user.hashed_password).unwrap();
        Argon2::default()
            .verify_password(body.password.as_bytes(), &parsed_hash)
            .unwrap();

        session.set("user_id", user.id);

        Ok(PlainText("Success".to_string()))
    }

    #[oai(path = "/logout", method = "post")]
    async fn logout(&self, session: &Session) -> poem::Result<PlainText<String>> {
        session.remove("user_id");
        Ok(PlainText("Logged out".to_string()))
    }

    #[oai(path = "/me", method = "get")]
    async fn me(&self, session: &Session, db: Data<&DB>) -> poem::Result<Json<User>> {
        let user_id: i32 = session.get("user_id").unwrap();
        let user = sqlx::query_as!(
            User,
            "select id, username, email from users where id = $1",
            user_id
        )
        .fetch_one(db.0)
        .await
        .unwrap();
        Ok(Json(user))
    }

    #[oai(path = "/check-username", method = "get")]
    async fn check_username(
        &self,
        query: Query<Username>,
        db: Data<&DB>,
    ) -> poem::Result<Json<bool>> {
        let row = sqlx::query!(
            "select 1 as exists from users where username = $1",
            query.username
        )
        .fetch_optional(db.0)
        .await
        .unwrap();
        Ok(Json(row.is_none()))
    }
}

#[derive(Object)]
pub struct UserSignup {
    email: String,
    username: String,
    password: String,
}

#[derive(Object)]
pub struct UserLogin {
    // email or username
    login: String,
    password: String,
}

#[derive(Object)]
pub struct User {
    id: i32,
    email: String,
    username: String,
}

#[derive(Object)]
pub struct UserSignupError {
    email: String,
    username: String,
}

#[derive(Object, Deserialize)]
pub struct Username {
    username: String,
}
