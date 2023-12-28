module Citadels.Districts where

import Citadels.Prelude
import Citadels.Types 

-- all districts minus the unique ones:
--

districts :: List District
districts = 
  [ District { set = Base, suit = Yellow, count = 3, cost = 5, name = "Palace" }
  , District { set = Base, suit = Yellow, count = 4, cost = 4, name = "Castle" }
  , District { set = Base, suit = Yellow, count = 5, cost = 3, name = "Manor" }
  , District { set = Base, suit = Yellow, count = 5, cost = 3, name = "Manor" }
  , District { set = Base, suit = Red, count = 2, cost = 5, name = "Fortress" }
  , District { set = Base, suit = Red, count = 3, cost = 3, name = "Baracks" }
  , District { set = Base, suit = Red, count = 3, cost = 2, name = "Prison" }
  , District { set = Base, suit = Red, count = 3, cost = 1, name = "Watchtower" }
  , District { set = Base, suit = Blue, count = 2, cost = 5, name = "Cathedral" }
  , District { set = Base, suit = Blue, count = 3, cost = 3, name = "Monastery" }
  , District { set = Base, suit = Blue, count = 3, cost = 2, name = "Church" }
  , District { set = Base, suit = Blue, count = 3, cost = 1, name = "Temple" }
  , District { set = Base, suit = Green, count = 2, cost = 5, name = "Town Hall" }
  , District { set = Base, suit = Green, count = 3, cost = 4, name = "Harbor" }
  , District { set = Base, suit = Green, count = 3, cost = 3, name = "Docks" }
  , District { set = Base, suit = Green, count = 4, cost = 2, name = "Market" }
  , District { set = Base, suit = Green, count = 3, cost = 2, name = "Trading Post" }
  , District { set = Base, suit = Green, count = 5, cost = 1, name = "Tavern" }
  , District { count = 1, suit = Purple, set = Base, name = "Haunted Quarter", cost = 2 }
  , District { count = 1, suit = Purple, set = Base, name = "Dragon Gate", cost = 6 }
  , District { count = 1, suit = Purple, set = Base, name = "Keep", cost = 6 }
  , District { count = 1, suit = Purple, set = Base, name = "Library", cost = 6 }
  , District { count = 1, suit = Purple, set = Base, name = "School of Magic", cost = 6 }
  , District { count = 1, suit = Purple, set = Base, name = "Observatory", cost = 4 }
  , District { count = 1, suit = Purple, set = Base, name = "Great Wall", cost = 6 }
  , District { count = 1, suit = Purple, set = Base, name = "Smithy", cost = 5 }
  , District { count = 1, suit = Purple, set = Base, name = "Laboratory", cost = 5 }
  , District { count = 1, suit = Purple, set = DarkCity, name = "Armory", cost = 3 }
  , District { count = 1, suit = Purple, set = DarkCity, name = "Quarry", cost = 5 }
  , District { count = 1, suit = Purple, set = DarkCity, name = "Poor House", cost = 4 }
  , District { count = 1, suit = Purple, set = DarkCity, name = "Mueseum", cost = 4 }
  , District { count = 1, suit = Purple, set = DarkCity, name = "Factory", cost = 5 }
  , District { count = 1, suit = Purple, set = DarkCity, name = "Map Room", cost = 5 }
  , District { count = 1, suit = Purple, set = DarkCity, name = "Park", cost = 6 }
  , District { count = 1, suit = Purple, set = DarkCity, name = "Imperial Treasury", cost = 5 }
  , District { count = 1, suit = Purple, set = DarkCity, name = "Wishing Well", cost = 5 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Necropolis", cost = 5 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Statue", cost = 3 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Stables", cost = 2 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Monument", cost = 4 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Theater", cost = 6 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Thieves' Den", cost = 6 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Ivory Tower", cost = 5 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Basilica", cost = 4 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Capitol", cost = 5 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Framework", cost = 3 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Gold Mine", cost = 6 }
  , District { count = 1, suit = Purple, set = Citadels2016, name = "Secret Vault", cost = 1_000_000 }
  ]
