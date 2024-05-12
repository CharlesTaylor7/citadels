use crate::actions::CityDistrictTarget;
use crate::{districts::DistrictName, types::PlayerName};
use serde::de;
use serde::ser::{Serialize, Serializer};
use std::fmt;

impl Serialize for CityDistrictTarget {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let item = self;
        format!("{:?},{},{}", item.district, item.player, item.beautified).serialize(serializer)
    }
}

impl<'de> de::Deserialize<'de> for CityDistrictTarget {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_str(Visitor)
    }
}

struct Visitor;

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
        if let Ok(Some(("district", value))) = value.next_entry::<&'de str, &'de str>() {
            self.visit_str(value)
        } else {
            Err(<<T as de::MapAccess<'de>>::Error as de::Error>::custom(
                "failure",
            ))
            // ::custom(value))
        }
    }

    fn visit_str<E>(self, csv: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
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
