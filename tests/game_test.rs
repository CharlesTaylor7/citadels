use citadels::actions::{Action, CityDistrictTarget};
use citadels::game::Game;
use citadels::lobby::{GameConfig, Lobby};
use citadels::random::Prng;
use citadels::roles::RoleName;
use citadels::types::PlayerName;
use rand::seq::SliceRandom;
use rand::Rng;
use rand_core::SeedableRng;
use std::borrow::{Borrow, BorrowMut};

fn start_with_entropy(lobby: Vec<&str>) -> Game {
    let lobby = Lobby::demo(lobby);
    let config = GameConfig::base_set();
    let roles = config
        .select(rand::thread_rng().borrow_mut(), lobby.players.len())
        .collect();
    Game::start(lobby.players, roles, SeedableRng::from_entropy())
}

#[test]
fn test_complete_city_size() {
    let game = start_with_entropy(vec!["alph", "brittany", "charlie"]);
    assert_eq!(game.complete_city_size(), 8);

    let game = start_with_entropy(vec!["alph", "brittany", "charlie", "dana"]);
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
