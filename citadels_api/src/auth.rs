use poem::Request;
use poem_openapi::{SecurityScheme, auth::Bearer};

pub struct User {
    name: String,
}

#[derive(SecurityScheme)]
#[oai(ty = "bearer", checker = "parse_token")]
struct MyUserAuthorization(User);

async fn parse_token(req: &Request, bearer: Bearer) -> Option<User> {
    todo!()
}
