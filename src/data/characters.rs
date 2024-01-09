use crate::actions::ActionTag::{self, *};
use crate::roles::RoleData;
use crate::roles::RoleName::{self, *};
use crate::types::CardSet::*;
use crate::types::CardSuit::*;

pub const ROLES: [RoleData; 27] =
  [ RoleData 
    { rank: 1 
    , set: Base
    , suit: None
    , name: Assassin
    , actions: &[(1, Assassinate)]
    , description: "Call a character you wish to kill. The killed character skips their turn."
    , reminder: ""
    }
  , RoleData 
    { rank: 1 
    , set: DarkCity
    , suit: None
    , name: Witch
    , actions: &[(1, Bewitch)]
    , description: "Gather resources, call a character you wish to bewitch, then put your turn on hold. After the bewitched character gathers resources, you resume your turn as that character."
    , reminder: "Gathering resources becomes a forced first step of your turn and of the bewitched player's turn. You gain both most active and passive abilities from the target, i.e. architect increased build limit, and bishop's protection from warlord. But you still rely on your own hand , own city and own gold. If you bewitch the King or Patrician, then they still get the crown. But you can use their other ability.
    You can bewitch the Emperor as normal. " 
    }
  , RoleData 
    { rank: 1 
    , set: Citadels2016
    , suit: None
    , name: Magistrate
    , actions: &[(3, SendWarrants)]
    , description: "Assign warrants to character cards. Reveal the signed warrant to confiscate the first district that player builds. The player gets back all gold paid to build that district."
    , reminder: "There are 3 warrants, 1 signed and 2 unsigned. Revealing the signed warrant is optional."
    }
  , RoleData 
    { rank: 2 
    , set: Base
    , suit: None
    , name: Thief
    , actions: &[(1, Steal)]
    , description: "Call a character you wish to rob. When the robbed character is revealed you take all their gold."
    , reminder: ""
    }

  , RoleData 
    { rank: 2 
    , set: Citadels2016 
    , suit: None 
    , name: RoleName::Spy
    , actions: &[(1, ActionTag::Spy)]
    , description: "Name a district type and look at another player's hand. For each card of that type, take 1 of their gold and gain 1 card." 
    , reminder: "You gain cards from the deck, not their hand."
    }
  , RoleData 
    { rank: 2 
    , set: Citadels2016 
    , suit: None 
    , name: RoleName::Blackmailer
    , actions: &[(2, Threaten)]
    , description: "Assign threats facedown to character cards. A threatened player can bribe you (half their gold rounded down) to remove their threat. If you reveal the flower, you take all their gold." 
    , reminder: "There are two threat markers, 1 flowered, and 1 normal. Revealing the normal marker has no effect. A threatened player must gather resources first on their turn and then decide whether to bribe the blackmailer. If not bribed, the blackmailer then decides if they want to reveal the marker. The cost of bribery is 0, if the player has only 1 gold."
    }
  , RoleData 
    { rank: 3 
    , set: Base 
    , suit: None 
    , name: Magician 
    , actions: &[(1, Magic)]
    , description: "Either exchange hands of cards with another player or discard any number of cards to gain an equal number of cards." 
    , reminder: ""
    }
  , RoleData 
    { rank: 3 
    , set: DarkCity 
    , suit: None 
    , name: Wizard 
    , actions: &[(1, WizardPeek)]
    , description: "Look at another player's hand and choose 1 card. Either pay to build it immediately or add it to your hand. You can build identical districts." 
    , reminder: ""
    }
  , RoleData 
    { rank: 3 
    , set: Citadels2016 
    , suit: None 
    , name: Seer
    , actions: &[(1, SeerTake)]
    , description: "Randomly take 1 card from each player's hand and add it to yours. Then give each player you took a card from 1 card from your hand. You can build up to 2 districts." 
    , reminder: "If someone's hand was empty, you don't give them a card."
    }
  , RoleData 
    { rank: 4 
    , set: Base
    , suit: Some(Noble)
    , name: King
    , actions: &[(1, GoldFromNobility)]
    , description: "Take the crown. Gain 1 gold for each of your NOBLE districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 4 
    , set: DarkCity
    , suit: Some(Noble)
    , name: Emperor
    , actions: &[(1, EmperorGiveCrown), (1, GoldFromNobility)]
    , description: "Give the crown to a different player and take either 1 of their gold or 1 of their cards. Gain 1 gold for each of your NOBLE districts."
    , reminder: "If killed, they still give the crown at the end of the round to another player. But they do not take any resources from the player. "
    }
  , RoleData 
    { rank: 4 
    , set: Citadels2016
    , suit: Some(Noble)
    , name: Patrician
    , actions: &[(1, CardsFromNobility)]
    , description: "Take the crown. Gain 1 card for each of your NOBLE districts."
    , reminder: ""
    }

  , RoleData 
    { rank: 5 
    , set: Base
    , suit: Some(Religious)
    , name: Bishop
    , actions: &[(1, GoldFromReligion)]
    , description: "The rank 8 character cannot use its ability on your districts. Gain 1 gold for each of your RELIGIOUS districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 5 
    , set: DarkCity
    , suit: Some(Religious)
    , name: Abbot
    , actions: &[(1, TakeFromRich), (1, ResourcesFromReligion)]
    , description: "The richest player gives you 1 gold. Gain either 1 gold or 1 card for each of your RELIGIOUS districts."
    , reminder: "If you are the richest or tied for richest, you can't use the first action.
    if there is tie amongst the richest players, you can pick who to take from.
    For the second action, you can take both gold and cards. The total just needs to add up to
    your number of religious districts."
    }
  , RoleData 
    { rank: 5 
    , set: Citadels2016
    , suit: Some(Religious)
    , name: Cardinal
    , actions: &[ (1, CardsFromReligion)]
    , description: "If you are short of gold to build a district, exchange your cards for another player's gold ( 1 card : 1 gold). Gain 1 card for each of your RELIGIOUS districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 6 
    , set: Base
    , suit: Some(Trade)
    , name: Merchant
    , actions: &[(1, MerchantGainOneGold), (1, GoldFromTrade)]
    , description: "Gain 1 extra gold. Gain 1 gold for each of your TRADE districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 6 
    , set: DarkCity
    , suit: None
    , name: Alchemist
    , actions: &[]
    , description: "At the end of your turn, you get back all the gold you paid to build districts this turn. You cannot pay more gold than you have."
    , reminder: "You don't get back gold spent for other reasons, e.g. tax collector"
    }
  , RoleData 
    { rank: 6 
    , set: Citadels2016
    , suit: Some(Trade)
    , name: Trader
    , actions: &[(1, GoldFromTrade)]
    , description: "You can build any number of TRADE districts. Gain 1 gold for each of your TRADE districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 7 
    , set: Base
    , suit: None
    , name: Architect
    , actions: &[(1, ArchitectGainCards)]
    , description: "Gain 2 extra cards. You can build up to 3 districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 7 
    , set: DarkCity
    , suit: None
    , name: Navigator
    , actions: &[(1, NavigatorGain)]
    , description: "Gain either 4 extra gold or 4 extra cards. You cannot build any districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 7 
    , set: Citadels2016
    , suit: None
    , name: Scholar
    , actions: &[(1, ScholarReveal)]
    , description: "Draw 7 cards, choose 1 to keep, then shuffle the rest back into the deck. You can build up to 2 districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 8 
    , set: Base
    , suit: Some(Military)
    , name: Warlord
    , actions: &[(1, GoldFromMilitary), (1, Destroy)]
    , description: "Destroy 1 district by paying 1 fewer gold than its cost. Gain 1 gold for each of your MILITARY districts."
    , reminder: "You cannot target a completed city. You cannot target the Bishop's city."
    }
  , RoleData 
    { rank: 8 
    , set: DarkCity
    , suit: Some(Military)
    , name: Diplomat
    , actions: &[(1, GoldFromMilitary), (1, ExchangeCityDistricts)]
    , description: "Exchange 1 of your districts for another player's district, giving them gold equal to the difference in their costs. Gain 1 gold for each of your MILITARY districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 8 
    , set: Citadels2016
    , suit: Some(Military)
    , name: Marshal
    , actions: &[(1, GoldFromMilitary), (1, Seize)]
    , description: "Seize 1 district with a cost of 3 or less from another player's city, giving that player gold equal to its cost. Gain 1 gold for each of your MILITARY districts."
    , reminder: ""
    }
  , RoleData 
    { rank: 9 
    , set: Citadels2016
    , suit: None
    , name: Queen
    , actions: &[(1, QueenGainGold)]
    , description: "If you are sitting next to the rank 4 character, gain 3 gold."
    , reminder: "Only available in 5+ player games."
    }
  , RoleData 
    { rank: 9 
    , set: DarkCity
    , suit: None
    , name: Artist
    , actions: &[(2, Beautify)]
    , description: "Beautify up to 2 of your districts by assigning each of them 1 of your gold. A district can be beautified only once. "
    , reminder: "A beautified district is worth 1 more point and its cost is raised by 1 gold. For example, the Warlord has to pay 1 more to destroy a beautified city."
    }
  , RoleData 
    { rank: 9 
    , set: Citadels2016
    , suit: None
    , name: TaxCollector
    , actions: &[(1, CollectTaxes)]
    , description: "After each player builds, they place 1 of their gold on the Tax Collector's character card. Take all gold from character card."
    , reminder: "The Tax Collector is not taxed. Taxes are always collected, even if the Tax Collector was killed, or discarded during the draft. In 2-3 player games, you are taxed when playing as your non-Tax Collector character."
    }
  ];

#[cfg(test)]
mod tests {
    use super::ROLES;

    #[test]
    pub fn test_role_names_align_with_character_data() {
        for (index, c) in ROLES.iter().enumerate() {
            assert_eq!(c.name as usize, index);
            assert_eq!(c.rank as usize, index / 3 + 1);
        }
    }
}
