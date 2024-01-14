use citadels::game::*;
use citadels::lobby::Lobby;
use citadels::types::*;

#[test]
fn test_complete_city_size() {
    let lobby = Lobby::demo(vec!["alph", "brittany", "charlie"]);
    let game = Game::start(lobby);
    assert_eq!(game.complete_city_size(), 8);

    let lobby = Lobby::demo(vec!["alph", "brittany", "charlie", "dana"]);
    let game = Game::start(lobby);
    assert_eq!(game.complete_city_size(), 7);
}
