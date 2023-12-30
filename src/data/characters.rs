use crate::types::CardSet::*;

use crate::types::Character;

// TODO:
// pub const CHARACTERS : [[Character; 3]; 9] =

pub const CHARACTERS : [Character; 9] =
  [ Character 
    { rank : 1 
    , set : Base
    , name : "Assassin"
    , description : "Call a character you wish to kill. The killed character skips their turn."
    }

  , Character 
    { rank : 2 
    , set : Base
    , name : "Thief"
    , description : "Call a character you wish to rob. When the robbed character is revealed you take all their gold."
    }
  , Character 
    { rank : 3 
    , set : Base
    , name : "Magician"
    , description : "Either exchange hands of cards with another player or discard any number of cards to gain an equal number of cards."
    }
  , Character 
    { rank : 4 
    , set : Base
    , name : "King"
    , description : "Take the crown. Gain 1 gold for each of your NOBLE districts."
    }
  , Character 
    { rank : 5 
    , set : Base
    , name : "Bishop"
    , description : "The Warlord/Marshall/Diplomat cannot uses its ability on your districts. Gain 1 gold for each of your RELIGIOUS districts."
    }
  , Character 
    { rank : 6 
    , set : Base
    , name : "Merchant"
    , description : "Gain 1 extra gold. Gain 1 gold for each of your TRADE districts."
    }
  , Character 
    { rank : 7 
    , set : Base
    , name : "Architect"
    , description : "Gain 2 extra cards. You can build up to 3 districts."
    }
  , Character 
    { rank : 8 
    , set : Base
    , name : "Warlord"
    , description : "Destroy 1 district by paying 1 fewer gold than its cost. Gain 1 gold for each of your MILITARY districts."
    }
  , Character 
    { rank : 9 
    , set : DarkCity
    , name : "Artist"
    , description : "Beautify up to 2 of your districts by assigning each of them 1 of your gold. A district can be beautified only once. (A beautified district is worth 1 additional point at the end of the game. It is also treated as if its cost is 1 higher than printed,i.e. the Warlord has to pay 1 extra to destroy it.)"
    }
  ];
