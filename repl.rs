:dep rand_core 
:dep citadels = { path = "./citadels/" }

use citadels::game::*;
use citadels::lobby::*;
use rand_core::SeedableRng;


let lobby = Lobby::demo(3);
let game = Game::start(lobby, SeedableRng::from_entropy());


