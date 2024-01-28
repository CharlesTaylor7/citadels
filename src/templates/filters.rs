use crate::actions::ActionTag;
use crate::types::CardSuit;
use std::fmt::Debug;

pub fn debug<T: Debug>(item: &T) -> askama::Result<String> {
    Ok(format!("{:#?}", item))
}

pub fn class(item: &ActionTag) -> askama::Result<&'static str> {
    let cls = match item {
        ActionTag::EndTurn => "btn-error",
        ActionTag::Build => "btn-primary",
        ActionTag::GatherResourceGold => "btn-primary",
        ActionTag::GatherResourceCards => "btn-primary",
        _ => "btn-secondary",
    };
    Ok(cls)
}
pub fn def<'a>(t: &'a Option<&'static str>) -> askama::Result<&'a str> {
    let c: Option<&str> = t.as_deref();
    Ok(c.unwrap_or_default())
}

pub fn suit_bg_character(suit: &Option<CardSuit>) -> askama::Result<&'static str> {
    match suit.as_ref() {
        Some(suit) => suit_bg_color(suit),
        None => Ok("bg-neutral"),
    }
}

pub fn suit_bg_color(suit: &CardSuit) -> askama::Result<&'static str> {
    Ok(match suit {
        CardSuit::Military => "bg-suit-military",
        CardSuit::Religious => "bg-suit-religious",
        CardSuit::Noble => "bg-suit-noble",
        CardSuit::Trade => "bg-suit-trade",
        CardSuit::Unique => "bg-suit-unique",
    })
}

pub fn suit_decoration_color(suit: &CardSuit) -> askama::Result<&'static str> {
    Ok(match suit {
        CardSuit::Military => "decoration-suit-military",
        CardSuit::Religious => "decoration-suit-religious",
        CardSuit::Noble => "decoration-suit-noble",
        CardSuit::Trade => "decoration-suit-trade",
        CardSuit::Unique => "decoration-suit-unique",
    })
}

pub fn suit_border_color(suit: &CardSuit) -> askama::Result<&'static str> {
    Ok(match suit {
        CardSuit::Military => "border-suit-military",
        CardSuit::Religious => "border-suit-religious",
        CardSuit::Noble => "border-suit-noble",
        CardSuit::Trade => "border-suit-trade",
        CardSuit::Unique => "border-suit-unique",
    })
}
