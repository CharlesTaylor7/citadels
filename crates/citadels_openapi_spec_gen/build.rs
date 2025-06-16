use std::fs::File;
use std::io::Write;

use citadels_api::api::service;

fn main() {
    let api_service = service();
    let spec = api_service.spec();

    let mut file = File::create("../../public/openapi.json").unwrap();
    file.write_all(spec.as_bytes()).unwrap();
}
