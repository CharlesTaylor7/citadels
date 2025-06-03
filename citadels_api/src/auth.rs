use poem::Request;
use poem_openapi::{Object, SecurityScheme, auth::Bearer};

pub struct User {
    email: String,
    username: String,
}

#[allow(dead_code)]
#[derive(SecurityScheme)]
#[oai(ty = "bearer", checker = "parse_token")]
struct MyUserAuthorization(User);

#[allow(unused)]
async fn parse_token(req: &Request, bearer: Bearer) -> Option<User> {
    todo!()
}
