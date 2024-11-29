use rand::prelude::*;
use std::fmt::Debug;

// My game's deterministic prng to allow restarting the game from the seed
type RngImpl = rand_xoshiro::Xoshiro256PlusPlus;
pub type Seed = <RngImpl as SeedableRng>::Seed;

pub struct Prng {
    pub seed: Seed,
    rng: RngImpl,
}

impl Debug for Prng {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "rng omitted")
    }
}

impl RngCore for Prng {
    fn next_u32(&mut self) -> u32 {
        self.rng.next_u32()
    }
    fn next_u64(&mut self) -> u64 {
        self.rng.next_u64()
    }
    fn fill_bytes(&mut self, dest: &mut [u8]) {
        self.rng.fill_bytes(dest)
    }
    fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), rand_core::Error> {
        self.rng.try_fill_bytes(dest)
    }
}

impl SeedableRng for Prng {
    type Seed = Seed;
    fn from_seed(seed: Self::Seed) -> Self {
        Self {
            seed,
            rng: SeedableRng::from_seed(seed),
        }
    }
}
