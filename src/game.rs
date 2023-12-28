use std::collections::HashMap;

type PlayerId = String;

pub struct Player {
    playerId: PlayerId,
    username: String,
    gold: Int,
    hand: Vec<District>,
    city: Vec<District>,
    roles: Vec<Character>,
}

#[derive(Default)]
pub struct Game {
    players: HashMap<PlayerId, Player>,
    seatingOrder: Vector<PlayerId>,
    characters: Vector<Character>,
    crowned: PlayerId,
}

/*
{ players = mempty
, seatingOrder = fromList []
, characters = fromList []
, crowned = PlayerId ""
}
*/
