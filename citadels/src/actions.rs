mod deserializer;

use crate::game::Player;
use crate::types::{CardSuit, PlayerName};
use crate::{districts::DistrictName, roles::RoleName};
use macros::tag::Tag;
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::borrow::Cow;

#[serde_as]
#[derive(Serialize, Deserialize, Tag, Debug, Clone)]
#[tag(serde::Deserialize)]
#[serde(tag = "action")]
pub enum Action {
    DraftPick {
        role: RoleName,
    },
    DraftDiscard {
        role: RoleName,
    },
    GatherResourceGold,
    GatherResourceCards,
    GatherCardsPick {
        district: DistrictName,
    },
    Build(BuildMethod),
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
        district: CityDistrictTarget,
    },
    Beautify {
        district: CityDistrictTarget,
    },
    ScholarReveal,
    ScholarPick {
        district: DistrictName,
    },
    EmperorGiveCrown {
        player: PlayerName,
        resource: Resource,
    },
    EmperorHeirGiveCrown {
        player: PlayerName,
    },
    ResourcesFromReligion {
        #[serde_as(as = "serde_with::DisplayFromStr")]
        gold: usize,
        #[serde_as(as = "serde_with::DisplayFromStr")]
        cards: usize,
    },
    TakeFromRich {
        player: PlayerName,
    },
    CardsFromReligion,
    Bewitch {
        role: RoleName,
    },
    SendWarrants {
        signed: RoleName,
        unsigned: [RoleName; 2],
    },
    Blackmail {
        flowered: RoleName,
        unmarked: RoleName,
    },
    PayBribe,
    IgnoreBlackmail,
    RevealWarrant,
    RevealBlackmail,
    Pass,
    MarshalSeize {
        district: CityDistrictTarget,
    },
    CollectTaxes,
    DiplomatTrade {
        district: CityDistrictTarget,
        theirs: CityDistrictTarget,
    },
    Spy {
        player: PlayerName,
        suit: CardSuit,
    },
    SpyAcknowledge,
    QueenGainGold,
    NavigatorGain {
        resource: Resource,
    },
    SeerTake,
    SeerDistribute {
        #[serde_as(as = "serde_with::Map<_, _>")]
        #[serde(flatten)]
        seer: Vec<(PlayerName, DistrictName)>,
    },
    WizardPeek {
        player: PlayerName,
    },
    WizardPick(WizardMethod),
    Smithy,
    Laboratory {
        district: DistrictName,
    },
    Armory {
        district: CityDistrictTarget,
    },
    Museum {
        district: DistrictName,
    },
    Theater {
        role: RoleName,
        player: PlayerName,
    },
    TheaterPass,
}

#[serde_as]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "build_method")]
pub enum BuildMethod {
    Regular {
        district: DistrictName,
    },
    Framework {
        district: DistrictName,
    },
    Necropolis {
        sacrifice: CityDistrictTarget,
    },
    ThievesDen {
        #[serde_as(as = "serde_with::OneOrMany<_>")]
        discard: Vec<DistrictName>,
    },
    Cardinal {
        district: DistrictName,
        #[serde_as(as = "serde_with::OneOrMany<_>")]
        discard: Vec<DistrictName>,
        player: PlayerName,
    },
}

#[serde_as]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "wizard_method")]
pub enum WizardMethod {
    Pick {
        district: DistrictName,
    },
    Build {
        district: DistrictName,
    },
    Framework {
        district: DistrictName,
    },
    Necropolis {
        sacrifice: CityDistrictTarget,
    },
    ThievesDen {
        #[serde_as(as = "serde_with::OneOrMany<_>")]
        discard: Vec<DistrictName>,
    },
}
#[derive(Debug, Clone)]
pub struct CityDistrictTarget {
    pub player: PlayerName,
    pub district: DistrictName,
    pub beautified: bool,
}

impl CityDistrictTarget {
    pub fn effective_cost(&self) -> usize {
        self.district.data().cost + if self.beautified { 1 } else { 0 }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Resource {
    Gold,
    Cards,
}

#[serde_as]
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum MagicianAction {
    TargetPlayer {
        player: PlayerName,
    },
    TargetDeck {
        #[serde_as(as = "serde_with::OneOrMany<_>")]
        district: Vec<DistrictName>,
    },
}
impl Action {
    pub fn is_build(&self) -> bool {
        match self {
            Action::Build { .. } => true,
            Action::WizardPick(WizardMethod::Pick { .. }) => false,
            Action::WizardPick(_) => true,
            _ => false,
        }
    }
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

            ActionTag::CardsFromReligion => {
                let suit = CardSuit::Religious;
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
            ActionTag::SeerTake => "Seer".into(),
            ActionTag::WizardPeek => "Wizard".into(),
            ActionTag::MarshalSeize => "Seize".into(),
            ActionTag::DiplomatTrade => "Trade".into(),
            ActionTag::CollectTaxes => "Collect Taxes".into(),
            ActionTag::Bewitch => "Bewitch".into(),
            ActionTag::EmperorGiveCrown => "Grant Crown".into(),
            ActionTag::EmperorHeirGiveCrown => "Grant Crown".into(),
            ActionTag::Blackmail => "Blackmail".into(),
            ActionTag::Spy => "Spy".into(),
            ActionTag::GatherCardsPick => "Pick".into(),
            ActionTag::TheaterPass => "Pass".into(),
            ActionTag::SpyAcknowledge => "Acknowledge".into(),
            ActionTag::SeerDistribute => "Distribute".into(),
            ActionTag::WizardPick => "Pick".into(),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum ActionSubmission {
    Complete(Action),
    Incomplete { action: ActionTag },
}
