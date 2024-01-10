use std::{borrow::Cow, fmt::Debug};

use crate::{actions::ActionTag, types::CardSuit};

pub fn debug<T: Debug>(item: &T) -> askama::Result<String> {
    Ok(format!("{:#?}", item))
}

pub fn class(item: &ActionTag) -> askama::Result<&'static str> {
    let cls = match item {
        ActionTag::EndTurn => "btn-error",
        _ => "btn-secondary",
    };
    Ok(cls)
}

pub fn suit_bg_character(suit: &Option<CardSuit>) -> askama::Result<&'static str> {
    match suit.as_ref() {
        Some(suit) => suit_bg_color(suit),
        None => Ok("bg-neutral-content"),
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

pub fn def<'a>(t: &'a Option<&'static str>) -> askama::Result<&'a str> {
    let c: Option<&str> = t.as_deref();
    Ok(c.unwrap_or_default())
}
