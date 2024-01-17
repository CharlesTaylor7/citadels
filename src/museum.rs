use crate::districts::DistrictName;
use rand::seq::SliceRandom;
use std::{borrow::Borrow, fmt::Debug};

#[derive(Debug)]
pub struct Museum {
    artifacts: Vec<&'static str>,
    cards: Vec<DistrictName>,
}

impl Default for Museum {
    fn default() -> Self {
        Self {
            artifacts: vec![],
            cards: vec![],
        }
    }
}

impl Museum {
    pub fn into_cards(self) -> Vec<DistrictName> {
        self.cards
    }

    pub fn cards(&self) -> &[DistrictName] {
        self.cards.borrow()
    }

    pub fn artifacts(&self) -> &[&'static str] {
        self.artifacts[0..self.cards.len()].borrow()
    }

    pub fn tuck(&mut self, card: DistrictName) {
        self.cards.push(card);
        if self.cards.len() > self.artifacts.len() {
            let mut artifacts = Museum::ARTIFACTS;
            artifacts.shuffle(&mut rand::thread_rng());
            for artifact in artifacts {
                self.artifacts.push(artifact);
            }
        }
    }

    const ARTIFACTS: [&'static str; 21] = [
        "⚱️", "🏺", "🖼️", "🗿", "🏛️", "⛲", "⚖️", "🕰️", "🍌", "🦴", "🦾", "⚰️", "🚀", "🐾", "🦖", "🦣",
        "🦤", "🦕", "💎", "🪩", "🔱",
    ];
}
