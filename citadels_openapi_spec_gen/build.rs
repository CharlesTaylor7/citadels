use std::fs::File;
use std::io::Write;

use citadels_api::api::service;
use poem_openapi::OpenApiService;

fn main() {
    let api_service = service();
    let spec = api_service.spec();
    let json = serde_json::to_string_pretty(&spec).unwrap();

    let mut file = File::create("openapi.json").unwrap();
    file.write_all(json.as_bytes()).unwrap();
}
