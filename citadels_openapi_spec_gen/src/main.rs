// use citadels::districts::{DistrictName, NORMAL, UNIQUE};
use serde_json::Value;
use std::fs;

fn main() {
    // let districts = NORMAL.iter().chain(UNIQUE.iter()).collect::<Vec<_>>();
    //
    // let others: Value =
    //     serde_json::from_str(&fs::read_to_string("../public/districts.json").unwrap()).unwrap();
    // // others
    // //
    // //     .as_array()
    // //     .unwrap()
    // //     .iter()
    // //     .zip(districts)
    // //     .for_each(|(ts, rs)| {
    // //         let ai_description = ts
    // //             .as_object()
    // //             .unwrap()
    // //             .get("description")
    // //             .unwrap()
    // //             .as_str()
    // //             .unwrap_or("");
    // //         let my_description = rs.description.unwrap_or("");
    // //         if (ai_description != my_description) {
    // //             println!("Card: {}", rs.display_name);
    // //             println!("Mine: {}\nAI:{}", my_description, ai_description);
    // //         }
    // //     });
    // //
    //
    // let mut data = serde_json::to_value(&districts).unwrap();
    // data.as_array_mut()
    //     .unwrap()
    //     .iter_mut()
    //     .zip(others.as_array().unwrap())
    //     .for_each(|(row, ai)| {
    //         let district = row.as_object_mut().unwrap();
    //
    //         let name: DistrictName =
    //             serde_json::from_value(district.get("name").unwrap().clone()).unwrap();
    //         district.insert(
    //             "multiplicity".to_string(),
    //             serde_json::to_value(name.multiplicity()).unwrap(),
    //         );
    //
    //         district.insert(
    //             "id".to_string(),
    //             serde_json::to_value(name as usize).unwrap(),
    //         );
    //         if (ai.get("multiplicity").unwrap().as_u64().unwrap() != name.multiplicity() as u64) {
    //             println!("Ai screwed up multiplicity");
    //         }
    //     });
    //
    // fs::write(
    //     "../public/districts.json",
    //     serde_json::to_string(&data).unwrap(),
    // )
    // .unwrap();
}
