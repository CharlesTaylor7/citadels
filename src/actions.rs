use serde_with::{serde_as, DisplayFromStr};
pub mod deserializer;
use crate::actions::deserializer::*;
use crate::game::Player;
use crate::types::{CardSuit, PlayerName};
use crate::{districts::DistrictName, roles::RoleName};
use macros::tag::Tag;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

#[serde_as]
#[derive(Serialize, Deserialize, Tag, Debug, Clone)]
#[tag(serde::Deserialize)]
#[serde(tag = "action")]
pub enum Action {
    // single select a role, then click pick or discard
    // Draft Phase
    DraftPick {
        role: RoleName,
    },
    DraftDiscard {
        role: RoleName,
    },

    // Call phase
    GatherResourceGold,
    GatherResourceCards,
    GatherCardsPick {
        district: DistrictName,
    },

    Build {
        district: DistrictName,
        // to sacrifice
        #[serde(default)]
        framework: bool,
        // to sacrifice
        necropolis: Option<DistrictName>,
        // to discard
        #[serde(default)]
        thieves_den: Vec<DistrictName>,
    },
    // Happens automatically when no actions are left.
    // Turn can end early if requested
    EndTurn,
    GoldFromNobility,
    GoldFromReligion,
    GoldFromTrade,
    GoldFromMilitary,
    CardsFromNobility,
    MerchantGainOneGold,
    ArchitectGainCards,
    TakeCrown,
    Assassinate {
        role: RoleName,
    },
    Steal {
        role: RoleName,
    },
    Magic(MagicianAction),
    WarlordDestroy {
        #[serde(
            serialize_with = "serialize_city_district_target",
            deserialize_with = "deserialize_city_district_target"
        )]
        district: CityDistrictTarget,
    },
    Beautify {
        #[serde(
            serialize_with = "serialize_city_district_target",
            deserialize_with = "deserialize_city_district_target"
        )]
        district: CityDistrictTarget,
    },

    // reveals 7 cards
    ScholarReveal,
    // picks 1 of them
    // shuffles the rest into the deck.
    // Note: shuffle the _entire_ deck.
    // Usually cards are discarded to the bottom of the deck.
    ScholarPick {
        district: DistrictName,
    },

    // emperor always targets someone else
    // Similar to the king and patricain.
    // The action is required.
    // If bewitched, the witch assigns the crown.
    // If killed, the action occurs at the end of the round, and no resources are taken.
    EmperorGiveCrown {
        player: PlayerName,
    },

    // Abbot
    ResourcesFromReligion {
        #[serde_as(as = "DisplayFromStr")]
        gold: usize,
        #[serde_as(as = "DisplayFromStr")]
        cards: usize,
    },

    // Abbot
    // player arg to break a tie amongst the richest.
    TakeFromRich {
        player: PlayerName,
    },

    // Cardinal
    CardsFromReligion,

    // Witch
    Bewitch {
        role: RoleName,
    },

    // Magistrate
    SendWarrants {
        signed: RoleName,
        unsigned: [RoleName; 2],
    },

    // Blackmailer
    Blackmail {
        flowered: RoleName,
        unmarked: RoleName,
    },
    PayBribe,
    IgnoreBlackmail,

    RevealWarrant,
    RevealBlackmail,
    Pass,

    // Marshal
    Seize {
        #[serde(
            serialize_with = "serialize_city_district_target",
            deserialize_with = "deserialize_city_district_target"
        )]
        district: CityDistrictTarget,
    },

    // Tax Collector collects from the tax pool
    CollectTaxes,

    // Diplomat
    ExchangeCityDistricts, //{ exchange: [CityDistrictTarget; 2], },

    // Spy
    Spy {
        player: PlayerName,
        suit: CardSuit,
    },
    // followup to acknowledge they were able to peek at the hand of cards
    SpyAcknowledge,

    // can happen during turn, or at end of round if king was killed
    QueenGainGold,

    // 4 gold or 4 cards
    NavigatorGain {
        resource: Resource,
    },

    // take 1 card at random from each player
    SeerTake,
    // distribute 1 card to each player that you took from.
    // note: if an opponent has no cards initially, they get no card back from you.
    SeerDistribute {
        //seer: Vec<SeerTarget>,
    },

    // action
    // Reveals a players hand
    WizardPeek {
        player: PlayerName,
    },
    // picks out of revealed hand
    // gets the option to build the picked card or to add to hand
    WizardPick {
        district: DistrictName,
        build: bool,
    },

    // district actions
    // 2 gold -> 3 cards
    Smithy,
    // 1 card -> 2 gold
    Laboratory {
        district: DistrictName,
    },
    // destroy self, to destroy a district.
    // unaffected by great wall, and bishop.
    // can't destroy the keep.
    Armory {
        #[serde(
            serialize_with = "serialize_city_district_target",
            deserialize_with = "deserialize_city_district_target"
        )]
        district: CityDistrictTarget,
    },
    // store a card from hand under museum for 1 point at the end of game.
    Museum {
        district: DistrictName,
    },
    Theater {
        role: RoleName,
        player: PlayerName,
    },
}

#[derive(Debug, Clone)]
pub struct CityDistrictTarget {
    pub player: PlayerName,
    pub district: DistrictName,
    pub beautified: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Resource {
    Gold,
    Cards,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum Select<T> {
    One(T),
    Many(Vec<T>),
}

impl<T: Clone> Select<T> {
    pub fn to_vec(&self) -> Cow<'_, [T]>
    where
        [T]: ToOwned<Owned = Vec<T>>,
    {
        match self {
            Select::One(item) => Cow::Owned(vec![item.clone()]),
            Select::Many(items) => Cow::Borrowed(items),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Select::One(_) => 1,
            Select::Many(items) => items.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum MagicianAction {
    TargetPlayer { player: PlayerName },
    TargetDeck { district: Select<DistrictName> },
}

impl ActionTag {
    pub fn is_resource_gathering(self) -> bool {
        match self {
            ActionTag::GatherResourceGold => true,
            ActionTag::GatherResourceCards => true,
            _ => false,
        }
    }
    pub fn is_required(self) -> bool {
        match self {
            ActionTag::Bewitch => true,
            ActionTag::TakeCrown => true,
            ActionTag::EmperorGiveCrown => true,
            ActionTag::GatherResourceGold => true,
            ActionTag::GatherResourceCards => true,

            // followup actions are often required
            ActionTag::GatherCardsPick => true,
            ActionTag::SeerDistribute => true,
            ActionTag::ScholarPick => true,
            ActionTag::WizardPick => true,

            // response actions are required
            ActionTag::RevealWarrant => true,
            ActionTag::RevealBlackmail => true,
            ActionTag::Pass => true,
            ActionTag::PayBribe => true,
            ActionTag::IgnoreBlackmail => true,

            _ => false,
        }
    }

    pub fn label(&self, player: &Player) -> Cow<'_, str> {
        match self {
            ActionTag::GatherResourceGold => {
                let mut n = 2;
                if player.city_has(DistrictName::GoldMine) {
                    n += 1;
                }
                format!("Resource: Gain {} gold", n).into()
            }

            ActionTag::GatherResourceCards => {
                let has_lib = player.city_has(DistrictName::Library);
                let has_ob = player.city_has(DistrictName::Observatory);
                match (has_lib, has_ob) {
                    (true, true) => "Resource: Draw 3",
                    (true, false) => "Resource: Draw 2",
                    (false, true) => "Resource: Draw 3, pick 1",
                    (false, false) => "Resource: Draw 2, pick 1",
                }
                .into()
            }

            ActionTag::GoldFromTrade => {
                let suit = CardSuit::Trade;
                let count = player.count_suit_for_resource_gain(suit);
                format!("Gain {} gold from {}", count, suit).into()
            }
            ActionTag::GoldFromReligion => {
                let suit = CardSuit::Religious;
                let count = player.count_suit_for_resource_gain(suit);
                format!("Gain {} gold from {}", count, suit).into()
            }

            ActionTag::GoldFromMilitary => {
                let suit = CardSuit::Military;
                let count = player.count_suit_for_resource_gain(suit);
                format!("Gain {} gold from {}", count, suit).into()
            }

            ActionTag::GoldFromNobility => {
                let suit = CardSuit::Noble;
                let count = player.count_suit_for_resource_gain(suit);
                format!("Gain {} gold from {}", count, suit).into()
            }

            ActionTag::CardsFromNobility => {
                let suit = CardSuit::Noble;
                let count = player.count_suit_for_resource_gain(suit);
                format!("Gain {} cards from {}", count, suit).into()
            }

            ActionTag::ResourcesFromReligion => {
                let suit = CardSuit::Religious;
                let count = player.count_suit_for_resource_gain(suit);
                format!("Gain {} resources from {}", count, suit).into()
            }

            ActionTag::DraftPick => "Pick".into(),
            ActionTag::DraftDiscard => "Discard".into(),
            ActionTag::Build => "Build".into(),
            ActionTag::EndTurn => "End turn".into(),
            ActionTag::Assassinate => "Assassinate".into(),
            ActionTag::Steal => "Steal".into(),
            ActionTag::Magic => "Magic".into(),
            ActionTag::TakeCrown => "Take Crown".into(),
            ActionTag::MerchantGainOneGold => "Gain 1 extra gold".into(),
            ActionTag::ArchitectGainCards => "Gain 2 extra cards".into(),
            ActionTag::SendWarrants => "Send Warrants".into(),
            ActionTag::WarlordDestroy => "Destroy".into(),
            ActionTag::Beautify => "Beautify".into(),
            ActionTag::ScholarReveal => "Draw 7, pick 1".into(),
            ActionTag::ScholarPick => "Pick".into(),
            ActionTag::Museum => "Museum".into(),
            ActionTag::Smithy => "Smithy".into(),
            ActionTag::Theater => "Theater".into(),
            ActionTag::RevealWarrant => "Confiscate".into(),
            ActionTag::Pass => "Pass".into(),
            ActionTag::RevealBlackmail => "Reveal Blackmail".into(),
            ActionTag::PayBribe => "Pay bribe".into(),
            ActionTag::IgnoreBlackmail => "Ignore blackmail".into(),
            ActionTag::Armory => "Armory".into(),
            ActionTag::Laboratory => "Laboratory".into(),
            ActionTag::NavigatorGain => "Navigator".into(),
            ActionTag::QueenGainGold => "Queen".into(),
            ActionTag::TakeFromRich => "Take 1 gold from the richest".into(),
            _ => {
                log::debug!("Warning: default case for {:#?}", self);
                format!("{:#?}", self).into()
            }
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum ActionSubmission {
    Complete(Action),
    Incomplete { action: ActionTag },
}
