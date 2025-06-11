use citadels::districts::{NORMAL, UNIQUE};
use std::fs;
fn main() {
    let districts = NORMAL.iter().chain(UNIQUE.iter()).collect::<Vec<_>>();
    fs::write("districts.json", serde_json::to_string(&districts).unwrap());
}
