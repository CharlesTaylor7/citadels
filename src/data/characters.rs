use crate::actions::ActionTag::*;
use crate::roles::RoleData;
use crate::roles::RoleName::*;
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
    }
  , Witch.todo()
  , Magistrate.todo()
  , RoleData 
    { rank: 2 
    , set: Base
    , suit: None
    , name: Thief
    , actions: &[(1, Steal)]
    , description: "Call a character you wish to rob. When the robbed character is revealed you take all their gold."
    }
  , Spy.todo()
  , Blackmailer.todo()
  , RoleData 
    { rank: 3 
    , set: Base 
    , suit: None 
    , name: Magician 
    , actions: &[(1, MagicianSwap)]
    , description: "Either exchange hands of cards with another player or discard any number of cards to gain an equal number of cards." 
    }
  , Wizard.todo()
  , Seer.todo()
  , RoleData 
    { rank: 4 
    , set: Base
    , suit: Some(Royal)
    , name: King
    , actions: &[(1, GoldFromNobility)]
    , description: "Take the crown. Gain 1 gold for each of your NOBLE districts."
    }
  , Emperor.todo()
  , Patrician.todo()
  , RoleData 
    { rank: 5 
    , set: Base
    , suit: Some(Religious)
    , name: Bishop
    , actions: &[(1, GoldFromReligion)]
    , description: "The Warlord/Marshall/Diplomat cannot uses its ability on your districts. Gain 1 gold for each of your RELIGIOUS districts."
    }
  , Abbot.todo()
  , Cardinal.todo()
  , RoleData 
    { rank: 6 
    , set: Base
    , suit: Some(Trade)
    , name: Merchant
    , actions: &[(1, MerchantGainOneGold), (1, GoldFromTrade)]
    , description: "Gain 1 extra gold. Gain 1 gold for each of your TRADE districts."
    }
  , Alchemist.todo()
  , Trader.todo()
  , RoleData 
    { rank: 7 
    , set: Base
    , suit: None
    , name: Architect
    , actions: &[(1, ArchitectGainCards)]
    , description: "Gain 2 extra cards. You can build up to 3 districts."
    }
  , Navigator.todo()
  , Scholar.todo()
  , RoleData 
    { rank: 8 
    , set: Base
    , suit: Some(Military)
    , name: Warlord
    , actions: &[(1, GoldFromMilitary), (1, WarlordDestroy)]
    , description: "Destroy 1 district by paying 1 fewer gold than its cost. Gain 1 gold for each of your MILITARY districts."
    }
  , Diplomat.todo()
  , Marshal.todo()
  , Queen.todo()
  , RoleData 
    { rank: 9 
    , set: DarkCity
    , suit: None
    , name: Artist
    , actions: &[(2, ArtistBeautify)]
    , description: "Beautify up to 2 of your districts by assigning each of them 1 of your gold. A district can be beautified only once. (A beautified district is worth 1 more point and its cost is raised by 1 gold. For example, the Warlord has to pay 1 more to destroy a beautified city.)"
    }
  , TaxCollector.todo()
  ];

#[cfg(test)]
mod tests {
    use super::ROLES;

    #[test]
    pub fn test_role_names_align_with_character_data() {
        for (index, c) in ROLES.iter().enumerate() {
            assert_eq!(c.name as usize, index);
        }
    }
}
