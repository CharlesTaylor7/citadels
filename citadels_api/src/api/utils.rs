use crate::db::DB;
use crate::errors::RequestError;
use poem::web::Data;
use poem_openapi::ApiResponse;
use poem_openapi::Object;
use poem_openapi::OpenApi;
use poem_openapi::param::Path;
use poem_openapi::payload::{Json, PlainText};
use poem_openapi::types::ToJSON;

#[derive(ApiResponse)]
pub enum CreateResponse<T: Send + ToJSON> {
    #[oai(status = 201)]
    Created(Json<T>, #[oai(header = "location")] String),
}
