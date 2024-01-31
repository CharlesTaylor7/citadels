pub mod filters;
pub mod game;
pub mod lobby;

use std::borrow::Cow;

use crate::districts::DistrictName;
use crate::game::{CityDistrict, Game};
use crate::roles::{Rank, RoleName};
use crate::types::CardSuit;
use askama::Template;
use axum::response::Html;

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum GamePhase {
    Draft,
    Call,
}

#[derive(Clone)]
pub struct DistrictTemplate<'a> {
    pub enabled: bool,
    pub name: &'static str,
    pub cost: Option<usize>,
    pub value: String,
    pub suit: CardSuit,
    pub description: Option<&'static str>,
    pub beautified: bool,
    pub asset: ImageAssetTemplate,
    pub pos: Position,
    pub artifacts: Cow<'a, [&'static str]>,
}

#[derive(Clone, Default)]
pub struct Position {
    pub x: f64,
    pub y: f64,
    pub z: isize,
}

impl<'a> DistrictTemplate<'a> {
    pub fn from(district: DistrictName) -> Self {
        let data = district.data();
        let length = 170.0;
        let scale = 10.0;
        let (p_x, p_y) = Self::crop(district);
        let (brightness, saturate) = Self::lighting(district);
        let offset_x = p_x * length;
        let offset_y = p_y * length;

        let full_height = length * scale / 5.0;
        let full_width = length * (125.8 / 200.0) * (scale / 5.0);
        Self {
            enabled: true,
            name: data.display_name,
            cost: if district == DistrictName::SecretVault {
                None
            } else {
                Some(data.cost)
            },
            value: format!("{:#?}", district),
            suit: data.suit,
            description: data.description,
            beautified: false,
            pos: Position::default(),
            artifacts: Cow::Owned(vec![]),
            asset: ImageAssetTemplate {
                saturate,
                brightness,
                path: "/public/districts.jpeg",
                height: length,
                width: length,
                scale_percentage: scale * 100.0,
                offset_x: -offset_x + -full_width * (district as usize % 10) as f64,
                offset_y: -offset_y + -full_height * (district as usize / 10) as f64,
            },
        }
    }

    pub fn from_city(
        game: &'a Game,
        player_name: &str,
        index: usize,
        district: &CityDistrict,
    ) -> Self {
        let mut template = Self::from(district.name);
        template.beautified = district.beautified;
        template.pos.y = -185.0 * index as f64;
        template.value = format!("{},{},{}", template.value, player_name, district.beautified);

        if district.name == DistrictName::Museum {
            template.artifacts = game.museum.artifacts().into();
        }
        template
    }

    // x, y
    fn crop(district: DistrictName) -> (f64, f64) {
        match district {
            // yellow
            DistrictName::Manor => (0.236, 0.2),
            DistrictName::Palace => (0.236, 0.05),

            // blue
            DistrictName::Temple => (0.236, 0.3),
            DistrictName::Church => (0.236, 0.4),
            DistrictName::Monastery => (0.236, 0.7),
            DistrictName::Cathedral => (0.236, 0.7),

            // green
            DistrictName::Market => (0.236, 0.7),
            DistrictName::Tavern => (0.236, 0.4),
            DistrictName::TradingPost => (0.236, 0.7),
            DistrictName::Docks => (0.236, 0.5),
            DistrictName::Harbor => (0.236, 0.7),
            DistrictName::TownHall => (0.236, 0.6),

            // red
            DistrictName::Prison => (0.236, 0.3),
            DistrictName::Baracks => (0.236, 0.3),
            DistrictName::Fortress => (0.236, 0.15),

            DistrictName::Library => (0.27, 0.3),
            DistrictName::GoldMine => (0.236, 0.3),
            DistrictName::Statue => (0.236, 0.0),
            DistrictName::SchoolOfMagic => (0.236, 0.3),
            DistrictName::ImperialTreasury => (0.236, 0.3),
            DistrictName::Observatory => (0.236, 0.12),
            DistrictName::MapRoom => (0.236, 0.4),
            DistrictName::DragonGate => (0.236, 0.4),
            DistrictName::SecretVault => (0.236, 0.15),
            DistrictName::Quarry => (0.236, 0.5),
            DistrictName::HauntedQuarter => (0.236, 0.4),
            DistrictName::GreatWall => (0.236, 0.2),
            DistrictName::WishingWell => (0.236, 0.1),
            DistrictName::Park => (0.25, 0.0),
            DistrictName::Museum => (0.27, 0.1),
            DistrictName::IvoryTower => (0.236, 0.05),
            DistrictName::Laboratory => (0.236, 0.5),
            DistrictName::Theater => (0.236, 0.2),
            DistrictName::PoorHouse => (0.236, 0.5),
            DistrictName::Smithy => (0.236, 0.2),
            DistrictName::Framework => (0.236, 0.2),
            DistrictName::ThievesDen => (0.236, 0.3),
            DistrictName::Basilica => (0.236, 0.1),
            DistrictName::Monument => (0.236, 0.1),
            DistrictName::Factory => (0.236, 0.1),
            DistrictName::Capitol => (0.236, 0.1),
            _ => (0.236, 0.0),
        }
    }

    // brightness, saturate
    fn lighting(district: DistrictName) -> (f64, f64) {
        match district {
            // yellow
            DistrictName::Manor => (1.3, 1.0),
            DistrictName::Palace => (1.3, 1.0),

            // blue
            DistrictName::Temple => (1.3, 1.0),
            DistrictName::Church => (1.3, 1.0),
            DistrictName::Monastery => (1.3, 1.0),
            DistrictName::Cathedral => (1.5, 1.0),

            // green
            DistrictName::Market => (1.3, 1.0),
            DistrictName::Tavern => (1.3, 1.0),
            DistrictName::TradingPost => (1.3, 1.0),
            DistrictName::Docks => (1.5, 1.0),
            DistrictName::Harbor => (1.3, 1.0),
            DistrictName::TownHall => (1.3, 1.0),

            // red
            DistrictName::Prison => (1.5, 1.0),
            DistrictName::Baracks => (1.3, 1.0),
            DistrictName::Fortress => (1.5, 1.0),

            DistrictName::Library => (1.0, 1.0),
            DistrictName::GoldMine => (1.5, 1.0),
            DistrictName::Statue => (1.3, 1.0),
            DistrictName::SchoolOfMagic => (1.5, 1.0),
            DistrictName::ImperialTreasury => (1.5, 1.0),
            DistrictName::Observatory => (2.0, 1.0),
            DistrictName::MapRoom => (1.5, 1.0),
            DistrictName::DragonGate => (1.5, 1.0),
            DistrictName::SecretVault => (1.3, 1.0),
            DistrictName::Quarry => (1.3, 1.0),
            DistrictName::HauntedQuarter => (1.3, 1.0),
            DistrictName::GreatWall => (1.3, 1.0),
            DistrictName::WishingWell => (2.0, 2.0),
            DistrictName::Park => (1.2, 1.0),
            DistrictName::Museum => (1.2, 1.0),
            DistrictName::IvoryTower => (1.3, 1.0),
            DistrictName::Laboratory => (1.3, 1.0),
            DistrictName::Theater => (1.3, 1.0),
            DistrictName::PoorHouse => (1.3, 1.0),
            DistrictName::Smithy => (1.3, 1.0),
            DistrictName::Framework => (1.3, 1.0),
            DistrictName::ThievesDen => (1.3, 1.0),
            DistrictName::Basilica => (1.3, 1.0),
            DistrictName::Monument => (1.3, 1.0),
            DistrictName::Factory => (1.3, 1.0),
            DistrictName::Capitol => (1.3, 1.0),
            _ => (1.3, 1.0),
        }
    }
}

#[derive(Clone)]
pub struct ImageAssetTemplate {
    brightness: f64,
    saturate: f64,
    height: f64,
    width: f64,
    offset_x: f64,
    offset_y: f64,
    scale_percentage: f64,
    path: &'static str,
}

pub struct RoleTemplate {
    pub min_player_count: usize,
    pub enabled: bool,
    pub name: String,
    pub rank: Rank,
    pub value: String,
    pub suit: Option<CardSuit>,
    pub description: &'static str,
    pub asset: ImageAssetTemplate,
}

impl RoleTemplate {
    pub fn from(role: RoleName, height: f64) -> Self {
        let data = role.data();
        let width = height * 155.0 / 200.0;
        let full_height = height * 267.0 / 200.0;
        Self {
            min_player_count: role.min_player_count(),
            enabled: true,
            name: role.display_name(),
            rank: data.rank,
            value: format!("{:#?}", role),
            suit: data.suit,
            description: data.description,
            asset: ImageAssetTemplate {
                brightness: 1.0,
                saturate: 1.0,
                path: "/public/roles.jpeg",
                height,
                width,
                scale_percentage: 400.0,
                offset_x: -width * (role as usize % 10) as f64,
                offset_y: -full_height * (role as usize / 10) as f64,
            },
        }
    }
}

pub trait MyTemplate {
    fn to_html(&self) -> axum::response::Result<Html<String>>;
}

impl<T: Template> MyTemplate for T {
    fn to_html(&self) -> axum::response::Result<Html<String>> {
        match self.render() {
            Ok(html) => Ok(Html(html)),
            Err(err) => Err(format!("askama: {}", err).into()),
        }
    }
}

pub const DAISY_THEMES: [&'static str; 30] = [
    "light",
    "dark",
    "cupcake",
    "bumblebee",
    "emerald",
    "corporate",
    // "synthwave",
    "retro",
    // "cyberpunk",
    "valentine",
    "halloween",
    "garden",
    "forest",
    "aqua",
    "lofi",
    "pastel",
    "fantasy",
    "wireframe",
    "black",
    "luxury",
    "dracula",
    "cmyk",
    "autumn",
    "business",
    "acid",
    "lemonade",
    "night",
    "coffee",
    "winter",
    "dim",
    "nord",
    "sunset",
];
