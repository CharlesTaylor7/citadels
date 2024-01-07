use crate::game::Player;
use crate::types::{CardSuit, PlayerName};
use crate::{districts::DistrictName, roles::RoleName};
use macros::tag::Tag;
use serde::Deserialize;
use std::borrow::Cow;

#[derive(Deserialize, Tag, Debug)]
#[serde(tag = "action")]
pub enum Action {
    // single select a role, then click pick or discard
    // Draft Phase
    DraftPick { role: RoleName },
    DraftDiscard { role: RoleName },

    // Call phase

    // either gain gold or reveal cards
    GatherResources { resource: Resource },
    // pick 1 or more revealed cards
    GatherCardsPick { district: Select<DistrictName> },

    Build { district: DistrictName },
    // Happens automatically when no actions are left.
    // Turn can end early if requested
    EndTurn,

    // gold for district suits
    GoldFromNobility,
    GoldFromReligion,
    GoldFromTrade,
    GoldFromMilitary,
    MerchantGainOneGold,
    ArchitectGainCards,

    // the king and patrician always target themselves.
    // this action must happen each round.
    // The game says "at some point during their turn".
    // Since there's no strategy to waiting, it will happen automatically.
    // If bewitched the original player still claims the crown.
    // If the character is killed it happens at the end of the round.
    TakeCrown,

    Assassinate { role: RoleName },
    Steal { role: RoleName },

    // select 1 player, or select many cards from hand
    Magic(MagicianAction),

    // Warlord
    // may destroy own district
    // may not destroy from any completed city
    // may not destroy from bishop's city
    Destroy(CityDistrictTarget),

    // must be one of your district cities
    Beautify { district: DistrictName },

    // Patrician
    CardsFromNobility,

    // LATER

    // emperor always targets someone else
    // Similar to the king and patricain.
    // The action is required.
    // If bewitched, the witch assigns the crown.
    // If killed, the action occurs at the end of the round, and no resources are taken.
    EmperorAssignCrown { player: PlayerName },

    // Abbot
    ResourcesFromReligious { gold: usize, cards: usize },

    // Abbot
    TakeFromRich,

    // Cardinal
    CardsFromReligious,

    // Witch
    Bewitch { role: RoleName },

    // Magistrate
    SendWarrants { warrant: [Warrant; 3] },

    // Blackmailer
    Threaten { threat: [Threat; 2] },

    // Marshal
    Seize(CityDistrictTarget),

    // Tax Collector collects from the tax pool
    CollectTaxes,

    // Diplomat
    ExchangeCityDistricts { exchange: [CityDistrictTarget; 2] },

    // Spy
    Spy { player: PlayerName, suit: CardSuit },

    // can happen during turn, or at end of round if king was killed
    QueenGainGold,

    // 4 gold or 4 cards
    NavigatorGain { resource: Resource },

    // take 1 card at random from each player
    SeerTake,
    // distribute 1 card to each player that you took from.
    // note: if an opponent has no cards initially, they get no card back from you.
    SeerDistribute { seer: Vec<SeerTarget> },

    // reveals 7 cards
    ScholarReveal,
    // picks 1 of them
    // shuffles the rest into the deck.
    // Note: shuffle the _entire_ deck.
    // Usually cards are discarded to the bottom of the deck.
    ScholarPick { district: DistrictName },

    // action
    // Reveals a players hand
    WizardPeek { player: PlayerName },
    // picks out of revealed hand
    // gets the option to build the picked card or to add to hand
    WizardPick { district: DistrictName, build: bool },
}

// cardinal is complicated. I think I will add optional fields to the build action
// witch is complicated

#[derive(Deserialize, Debug)]
pub struct CityDistrictTarget {
    pub player: PlayerName,
    pub district: DistrictName,
    pub beautified: bool,
}

impl CityDistrictTarget {
    pub fn effective_cost(&self, player: &Player) -> usize {
        let mut cost = self.district.data().cost;
        if self.beautified {
            cost += 1;
        }

        if player
            .city
            .iter()
            .any(|city| city.name == DistrictName::GreatWall)
        {
            cost += 1;
        }

        cost
    }
}

#[derive(Deserialize, Debug)]
pub enum Resource {
    Gold,
    Cards,
}

#[derive(Deserialize, Debug)]
pub struct SeerTarget {
    pub player: PlayerName,
    pub district: DistrictName,
}

#[derive(Deserialize, Debug)]
pub struct Warrant {
    pub role: RoleName,
    pub signed: bool,
}

#[derive(Deserialize, Debug)]
pub struct Threat {
    pub role: RoleName,
    pub flowered: bool,
}

// action target domains:
// a role - Assassinate, Steal, single
// other player - Magician, single
// my hand - Magician, many
// my city - Build, beautify, single
// other city - Warlord, single
// revealed - picking cards revealed from somwhere else, such as top of deck or another hand,
// single or many
// draft - picking roles, single
//
// fluidly the ui needs to swap input types from 3 states:
// disabled = ""
// single = "checkbox"
// many = "radio"
//
// depending on what the active action is.
// I should not leak the UI needs into the domain layer.
// Resource pick cards is the only followup action I need at the moment.
// I may need more followup actions for new roles and districts, but at the moment that's the only
// followup needed.
//
// So I need the UI layer to smartly allow:
// - picking an action,
// - that enables selection from the desired zone/domain.
// - and puts in a simple confirm button into the ui.
//
//
//
#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum Select<T> {
    Single(T),
    Many(Vec<T>),
}

impl<T: Clone> Select<T> {
    pub fn to_vec(&self) -> Cow<'_, [T]>
    where
        [T]: ToOwned<Owned = Vec<T>>,
    {
        match self {
            Select::Single(item) => Cow::Owned(vec![item.clone()]),
            Select::Many(items) => Cow::Borrowed(items),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Select::Single(_) => 1,
            Select::Many(items) => items.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum MagicianAction {
    TargetPlayer { player: PlayerName },
    TargetDeck { discard: Select<DistrictName> },
}

impl ActionTag {
    pub fn is_required(self) -> bool {
        match self {
            ActionTag::Bewitch => true,
            ActionTag::TakeCrown => true,
            ActionTag::EmperorAssignCrown => true,
            ActionTag::GatherResources => true,

            // followup actions are often required
            ActionTag::GatherCardsPick => true,
            ActionTag::SeerDistribute => true,
            ActionTag::ScholarPick => true,
            ActionTag::WizardPick => true,

            _ => false,
        }
    }
}
