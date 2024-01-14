use citadels::actions::{Action, CityDistrictTarget};
use citadels::game::Game;
use citadels::lobby::Lobby;
use citadels::random::Prng;
use citadels::types::PlayerName;
use rand::seq::SliceRandom;
use rand::Rng;
use rand_core::SeedableRng;
use std::borrow::Borrow;

#[test]
fn test_complete_city_size() {
    let lobby = Lobby::demo(vec!["alph", "brittany", "charlie"]);
    let game = Game::start(lobby, SeedableRng::from_entropy());
    assert_eq!(game.complete_city_size(), 8);

    let lobby = Lobby::demo(vec!["alph", "brittany", "charlie", "dana"]);
    let game = Game::start(lobby, SeedableRng::from_entropy());
    assert_eq!(game.complete_city_size(), 7);
}

fn random_action(game: &Game, rng: &mut Prng) -> Action {
    todo!()
}

fn random_player<'a>(game: &'a Game, rng: &mut Prng) -> &'a PlayerName {
    game.players.choose(rng).unwrap().name.borrow()
}

fn random_city_district(game: &Game, rng: &mut Prng) -> Option<CityDistrictTarget> {
    let count = game.players.iter().map(|p| p.city.len()).sum();
    let n = rng.gen_range(0..count);
    let (player, district): (&PlayerName, _) = game
        .players
        .iter()
        .flat_map(|p| p.city.iter().map(|d| (p.name.borrow(), d)))
        .nth(n)?;

    Some(CityDistrictTarget {
        district: district.name,
        beautified: district.beautified,
        player: player.clone(),
    })
}
