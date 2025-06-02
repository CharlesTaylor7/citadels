//https://prng.di.unimi.it/
import crypto from "node:crypto";
import {
  type RandomGenerator,
  unsafeUniformIntDistribution,
  xoroshiro128plus,
} from "pure-rand";

/**
 * Shuffle an array in-place
 * @param array Array to shuffle
 * @param rng Random number generator
 */
export function shuffle<T>(array: T[], rng: RNG): void {
  const n = array.length;

  for (let i = n - 1; i > 0; i--) {
    const j = rng(i + 1);
    [array[i], array[j]] = [array[j], array[i]];
  }
}
export type RNG = (bound: number) => number;

export function systemRandom(max: number): number {
  return crypto.randomInt(max);
}

export type Seed = number;
export type PRNG = RandomGenerator;
export function newSeed(): Seed {
  return crypto.randomInt(2 ** 48 - 1);
}

export function newPrng(seed: Seed): PRNG {
  return xoroshiro128plus(seed);
}

export function asRng(prng: PRNG): RNG {
  return (bound) => unsafeUniformIntDistribution(0, bound - 1, prng);
}

export function deserializePrng(serializedRng: string): PRNG {
  const instance = xoroshiro128plus(0);
  Object.assign(instance, JSON.parse(serializedRng));
  return instance;
}

export function serializePrng(prng: PRNG) {
  return JSON.stringify(prng);
}

const PRNG_CONSTRUCTOR = "XoroShiro128Plus";

export function prngReplacer(key: string, value: unknown) {
  if (value?.constructor.name === PRNG_CONSTRUCTOR) {
    // eslint-disable-next-line
    (value as any).__type = PRNG_CONSTRUCTOR;
    return value;
  }
  return value;
}

export function prngReviver(key: string, value?: { __type?: string }) {
  if (value?.__type === PRNG_CONSTRUCTOR) {
    const instance = xoroshiro128plus(0);
    Object.assign(instance, value);
    return instance;
  }
  return value;
}
