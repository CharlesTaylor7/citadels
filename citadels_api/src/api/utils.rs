use poem_openapi::ApiResponse;
use poem_openapi::payload::Json;
use poem_openapi::types::ToJSON;

#[derive(ApiResponse)]
pub enum CreateResponse<T: Send + ToJSON> {
    #[oai(status = 201)]
    Created(Json<T>, #[oai(header = "location")] String),
}
