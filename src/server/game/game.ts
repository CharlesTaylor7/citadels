import { GameStartAction } from "@/core/actions";
import {
  DistrictName,
  DistrictNameUtils,
  NORMAL_DISTRICTS,
  UNIQUE_DISTRICTS,
} from "@/core/districts";
import { RoleName, RoleNameUtils, RANKS, ROLE_NAMES } from "@/core/roles";
import { CardSuit, PlayerId } from "@/core/types";
import {
  ActionOutput,
  CityDistrict,
  Draft,
  GameRole,
  GameState,
  Player,
} from "@/core/game";
import { ConfigOption } from "@/core/config";
import { asRng, newPrng, shuffle, RNG } from "@/server/game/random";

export function createPlayer(
  index: number,
  id: PlayerId,
  name: string,
): Player {
  return {
    index,
    id,
    name,
    gold: 2,
    hand: [],
    city: [],
    roles: [],
  };
}

export function createCityDistrict(name: DistrictName): CityDistrict {
  return {
    name,
    beautified: false,
  };
}

export function createCharacter(role: RoleName): GameRole {
  return {
    role,
    revealed: false,
    markers: [],
    logs: [],
  };
}

export function shuffleDeck(game: GameState): void {
  game.deck.push(...game.discard);
  game.discard = [];
  shuffle(game.deck, asRng(game.prng));
}

export function drawCard(game: GameState): DistrictName | null {
  if (game.deck.length === 0) {
    if (game.discard.length === 0) return null;
    game.deck = game.discard;
    game.discard = [];
    game.deck.reverse();
  }
  return game.deck.pop() ?? null;
}

export function drawCards(game: GameState, count: number): DistrictName[] {
  const cards: DistrictName[] = [];
  for (let i = 0; i < count; i++) {
    const card = drawCard(game);

    if (card !== null) {
      cards.push(card);
    }
  }
  return cards;
}

export function beginDraft(
  playerCount: number,
  playerIndex: number, // This is the crowned player who starts the draft
  roles: readonly RoleName[], // These are the roles selected for the game, already shuffled
  rng: RNG,
): Draft {
  const draftRoles = [...roles]; // Mutable copy

  const draft: Draft = {
    playerCount,
    playerIndex,
    remaining: draftRoles,
    theaterStep: false,
    initialDiscard: null,
    faceupDiscard: [],
  };

  const rolesForDraftRound = draft.remaining.length;

  // Number of roles to discard face up.
  // Rust: for _ in draft.player_count + 2..role_count
  // This means role_count - (draft.player_count + 2) cards are discarded face up.
  const numFaceupToDiscard = rolesForDraftRound - (playerCount + 2);

  if (playerCount >= 4 && numFaceupToDiscard > 0) {
    for (let i = 0; i < numFaceupToDiscard; i++) {
      if (draft.remaining.length === 0) break; // Should not happen if logic is correct

      let discardIndex = -1;
      // Try to find a role that can be discarded face up (not Rank Four)
      // Create a list of indices to try to ensure we check all if needed
      const tryOrder = draft.remaining.map((_, idx) => idx);
      shuffle(tryOrder, rng); // Shuffle order of checking

      for (const potentialIndex of tryOrder) {
        if (
          RoleNameUtils.data(draft.remaining[potentialIndex]).rank !== RANKS[3]
        ) {
          // RANKS[3] is "Four"
          discardIndex = potentialIndex;
          break;
        }
      }

      if (discardIndex === -1) {
        // Fallback: if all remaining are rank 4 (highly unlikely), or no suitable card found,
        // just pick one randomly. Rust's loop `loop { index = rng.gen_range... break; }`
        // implies it will always find one if one exists, or keep trying.
        // If all are rank 4, it would pick a rank 4.
        // To be safe, if no non-rank-4 is found, pick any.
        if (draft.remaining.length > 0) {
          discardIndex = rng(draft.remaining.length);
        } else {
          break; // No cards left to discard
        }
      }
      draft.faceupDiscard.push(draft.remaining.splice(discardIndex, 1)[0]);
    }
  }

  // Discard 1 card facedown
  if (draft.remaining.length > 0) {
    const facedownDiscardIndex = rng(draft.remaining.length);
    draft.initialDiscard = draft.remaining.splice(facedownDiscardIndex, 1)[0];
  }

  // Restore sort of roles by rank (primary) and then by original enum order (secondary for stability)
  draft.remaining.sort((a, b) => {
    const rankA = RoleNameUtils.data(a).rank;
    const rankB = RoleNameUtils.data(b).rank;
    if (rankA !== rankB) {
      return RANKS.indexOf(rankA) - RANKS.indexOf(rankB);
    }
    return ROLE_NAMES.indexOf(a) - ROLE_NAMES.indexOf(b);
  });

  return draft;
}

export function createGame(gameStartAction: GameStartAction): GameState {
  const {
    players: playerConfigs,
    config: gameOptions,
    rngSeed,
  } = gameStartAction;
  const prng = newPrng(rngSeed); // This is the PRNG object
  const gameRng = asRng(prng); // This is the RNG function derived from prng

  // Initialize and shuffle players for seating order
  const initialPlayerConfigs = [...playerConfigs]; // Clone to avoid mutating input
  shuffle(initialPlayerConfigs, gameRng);
  const players = initialPlayerConfigs.map((playerConfig, index) =>
    createPlayer(index, playerConfig.id, playerConfig.name),
  );

  // Role Selection from game configuration
  const selectedRolesForGame = Array.from(gameOptions.roles);
  shuffle(selectedRolesForGame, gameRng); // Shuffle the roles to be used in the game.

  const characters = selectedRolesForGame.map((role) => createCharacter(role));

  // Deck Construction
  const normalDistrictCards: DistrictName[] = [];
  NORMAL_DISTRICTS.forEach((districtData) => {
    const multiplicity = districtData.multiplicity;
    for (let i = 0; i < multiplicity; i++) {
      normalDistrictCards.push(districtData.name);
    }
  });

  const enabledUniqueDistricts = UNIQUE_DISTRICTS.filter(
    (districtData) =>
      gameOptions.districts.get(districtData.name) === ConfigOption.Enabled,
  ).map((d) => d.name);
  shuffle(enabledUniqueDistricts, gameRng);
  const selectedUniqueDistricts = enabledUniqueDistricts.slice(0, 14);

  const deck: DistrictName[] = [
    ...normalDistrictCards,
    ...selectedUniqueDistricts,
  ];
  shuffle(deck, gameRng);

  // Assertion for deck size, matches Rust's 68 for standard setup
  if (deck.length !== 68) {
    console.warn(
      `Deck size check failed: expected ${68}, got ${deck.length}. This might be due to custom district configurations.`,
    );
  }

  const crownedPlayerIndex = 0; // Player 0 (after shuffling) is initially crowned and starts the draft

  // Create the game state
  const game: GameState = {
    actions: [gameStartAction],
    players,
    characters,
    deck,
    discard: [],
    prng,
    museum: { artifacts: [], cards: [] },
    logs: [],
    crowned: crownedPlayerIndex,
    taxCollector: 0,
    alchemist: 0,
    firstToComplete: null,
    followup: null,
    notifications: [],
    activeTurn: {
      type: "Draft",
      draft: beginDraft(
        players.length,
        crownedPlayerIndex,
        selectedRolesForGame,
        gameRng,
      ),
    },
  };

  // Deal initial cards
  for (const player of players) {
    player.hand = drawCards(game, 4);
  }

  // Mark face-up discarded roles in the characters list
  if (game.activeTurn.type === "Draft") {
    game.activeTurn.draft.faceupDiscard.forEach((discardedRoleName) => {
      const char = game.characters.find((c) => c.role === discardedRoleName);
      if (char) {
        char.markers.push({ type: "Discarded" });
        char.revealed = true;
      }
    });
  }

  return game;
}

export function getActivePlayer(game: GameState): Player | null {
  const activeRole = getActiveRole(game);
  if (activeRole?.playerIndex == null) return null;

  return game.players[activeRole.playerIndex];
}

export function getActiveRole(game: GameState): GameRole | null {
  if (game.activeTurn.type === "Call") {
    return game.characters[game.activeTurn.call.index];
  }
  return null;
}

export function completeAction(
  game: GameState,
  playerIndex: number,
  cost: number,
  district: DistrictName,
): void {
  const player = game.players[playerIndex];
  player.gold -= cost;

  // Remove from hand
  const index = player.hand.findIndex((d) => d === district);
  if (index >= 0) {
    player.hand.splice(index, 1);
  }

  // Add to city
  player.city.push(createCityDistrict(district));

  // Track alchemist refund
  if (getActiveRole(game)?.role === "Alchemist") {
    game.alchemist += cost;
  }

  // Check for first to complete
  // The Rust version uses complete_city_size() which is 7 or 8.
  const citySizeTarget = game.players.length <= 3 ? 8 : 7; // Align with Rust's complete_city_size
  if (game.firstToComplete === null && player.city.length >= citySizeTarget) {
    game.firstToComplete = playerIndex;
  }
}

export function discardDistrict(game: GameState, district: DistrictName): void {
  // Rust version has special handling for Museum.
  if (district === "Museum") {
    const museumCards = [...game.museum.cards, "Museum" as DistrictName]; // Museum itself is a card
    shuffle(museumCards, asRng(game.prng!)); // Use game's prng
    game.discard.push(...museumCards);
    game.museum.cards = [];
    game.museum.artifacts = []; // Also clear artifacts if museum is discarded
  } else {
    game.discard.push(district);
  }
}

export function gainCards(game: GameState, amount: number): number {
  const player = getActivePlayer(game);
  if (!player) throw new Error("No active player to gain cards");

  const cards = drawCards(game, amount);
  player.hand.push(...cards);
  return cards.length;
}

export function gainGoldForSuit(game: GameState, suit: CardSuit): ActionOutput {
  const player = getActivePlayer(game);
  if (!player) throw new Error("no active player");
  const amount = countSuitForResourceGain(player, suit);

  player.gold += amount;

  const activeRole = getActiveRole(game); // getActiveRole can return null
  const roleName = activeRole
    ? RoleNameUtils.data(activeRole.role).name
    : "Current Role";
  return {
    log: `The ${roleName} (${player.name}) gains ${amount} gold from their ${suit} districts.`,
  };
}

export function gainCardsForSuit(
  game: GameState,
  suit: CardSuit,
): ActionOutput {
  const player = getActivePlayer(game);
  if (!player) throw new Error("no active player");
  const count = countSuitForResourceGain(player, suit);

  const amount = gainCards(game, count); // gainCards ensures activePlayer exists
  const activeRole = getActiveRole(game);
  const roleName = activeRole
    ? RoleNameUtils.data(activeRole.role).name
    : "Current Role";
  return {
    log: `The ${roleName} (${player.name}) gains ${amount} cards from their ${suit} districts.`,
  };
}

export function countSuitForResourceGain(
  player: Player,
  suit: CardSuit,
): number {
  return player.city.filter(
    (c) =>
      c.name === "SchoolOfMagic" ||
      DistrictNameUtils.data(c.name).suit === suit,
  ).length;
}
