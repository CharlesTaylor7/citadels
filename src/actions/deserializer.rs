use serde::de;
use serde::Deserialize;
use std::fmt;

use crate::{districts::DistrictName, types::PlayerName};

use super::CityDistrictTarget;

struct Visitor;

pub fn deserialize_city_district_target<'de, D>(
    deserializer: D,
) -> Result<CityDistrictTarget, D::Error>
where
    D: de::Deserializer<'de>,
{
    let csv = String::deserialize(deserializer)?;

    log::info!("visit_str");
    let mut district = None as Option<DistrictName>;
    let mut player = None as Option<PlayerName>;
    let mut beautified = None as Option<bool>;
    for (i, raw) in csv.split(",").enumerate() {
        if i == 0 {
            let result = serde_json::from_value(serde_json::Value::String(raw.to_owned()));
            log::info!("{raw} {result:#?}");
            district = result.ok()
        }

        if i == 1 {
            let result = serde_json::from_value(serde_json::Value::String(raw.to_owned()));
            log::info!("{raw} {result:#?}");
            player = result.ok();
        }

        if i == 2 {
            log::info!("{raw}");
            beautified = match raw {
                "true" => Some(true),
                "false" => Some(false),
                _ => None,
            };
        }
    }

    if let (Some(district), Some(player), Some(beautified)) = (district, player, beautified) {
        Ok(CityDistrictTarget {
            district,
            player,
            beautified,
        })
    } else {
        Err(de::Error::custom(csv))
    }
}
/*
// This is the trait that informs Serde how to deserialize MyMap.
impl<'de> de::Deserialize<'de> for CityDistrictTarget {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        // Instantiate our Visitor and ask the Deserializer to drive
        // it over the input data, resulting in an instance of MyMap.
        deserializer.deserialize_map(Visitor {})
    }
}
*/

impl<'de> de::Visitor<'de> for Visitor {
    type Value = CityDistrictTarget;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("A comma separated list: district,player,beautified")
    }

    fn visit_map<T>(self, mut value: T) -> Result<Self::Value, <T as de::MapAccess<'de>>::Error>
    where
        T: de::MapAccess<'de>,
        T::Error: de::Error,
    {
        log::info!("visit_map");
        if let Ok(Some(("district", value))) = value.next_entry::<&'de str, &'de str>() {
            self.visit_str(value)
        } else {
            Err(<<T as de::MapAccess<'de>>::Error as de::Error>::custom(
                "failure",
            ))
            // ::custom(value))
        }
    }

    fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        log::info!("visit_string");
        self.visit_str(&value)
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        log::info!("visit_str");
        let mut district = None as Option<DistrictName>;
        let mut player = None as Option<PlayerName>;
        let mut beautified = None as Option<bool>;
        for (i, raw) in value.split(",").enumerate() {
            if i == 0 {
                let result = serde_json::from_str(raw);
                log::info!("{raw} {result:#?}");
                district = result.ok()
            }

            if i == 1 {
                let result = serde_json::from_str(raw);
                log::info!("{raw} {result:#?}");
                player = result.ok();
            }

            if i == 2 {
                let result = serde_json::from_str(raw);
                log::info!("{raw} {result:#?}");
                beautified = result.ok();
            }
        }

        if let (Some(district), Some(player), Some(beautified)) = (district, player, beautified) {
            Ok(CityDistrictTarget {
                district,
                player,
                beautified,
            })
        } else {
            Err(E::custom(value))
        }
    }
}

#[derive(Debug)]
enum Error {
    #[allow(dead_code)]
    Custom { msg: String },
    /*
    District { raw: String },
    Player { raw: String },
    Beautified { raw: String },
    */
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for Error {}
impl de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Self::Custom {
            msg: msg.to_string(),
        }
    }
}
