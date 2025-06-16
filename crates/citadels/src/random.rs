use rand::SeedableRng;

pub type Prng = rand_xoshiro::Xoshiro256PlusPlus;
pub type Seed = <Prng as SeedableRng>::Seed;

pub fn seed_from_entropy() -> Seed {
    let mut seed = Seed::default();
    getrandom::getrandom(&mut seed).expect("entropy failed");
    seed
}
