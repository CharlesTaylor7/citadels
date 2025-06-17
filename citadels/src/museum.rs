use crate::districts::DistrictName;
use rand::seq::SliceRandom;
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, fmt::Debug};

#[derive(Debug, Serialize, Deserialize)]
pub struct Museum {
    artifacts: Vec<String>,
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

    pub fn artifacts(&self) -> &[String] {
        self.artifacts[0..self.cards.len()].borrow()
    }

    pub fn tuck(&mut self, card: DistrictName) {
        self.cards.push(card);
        if self.cards.len() > self.artifacts.len() {
            let mut artifacts = Museum::ARTIFACTS;
            artifacts.shuffle(&mut rand::thread_rng());
            self.artifacts
                .extend(artifacts.iter().map(|a| a.to_string()));
        }
    }

    const ARTIFACTS: [&'static str; 19] = [
        "⚱️", "🏺", "🖼️", "🗿", "🏛️", "⛲", "🕰️", "🦴", "🦾", "⚰️", "🚀", "🦖", "🦣", "🦤", "🦕",
        "💎", "🪩", "🔱", "🧋",
    ];
}
