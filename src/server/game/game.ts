import { GameStartAction, PlayerAction } from "@/core/actions";
import {
  DistrictName,
  DISTRICT_NAMES,
  DistrictNameUtils,
} from "@/core/districts";
import { RoleName, ROLE_NAMES } from "@/core/roles";
import { CardSuit, PlayerId } from "@/core/types";
import {
  ActionOutput,
  CityDistrict,
  Draft,
  GameRole,
  GameState,
  Player,
} from "@/core/game";
import { GameConfig } from "@/core/config";
import { asRng, newPrng, shuffle } from "@/server/game/random";

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

// TODO: aider
export function beginDraft(
  playerCount: number,
  playerIndex: number,
  roles: readonly RoleName[],
): Draft {
  const draft: Draft = {
    playerCount,
    playerIndex,
    remaining: [...roles],
    theaterStep: false,
    initialDiscard: null,
    faceupDiscard: [],
  };

  return draft;
}

export function createGame(config: GameStartAction): GameState {
  // Initialize players
  const players = config.players.map((player, index) =>
    createPlayer(index, player.id, player.name),
  );
  const prng = newPrng(config.rngSeed);

  // Initialize characters
  // TODO: aider
  const characters = ROLE_NAMES.map((role) => createCharacter(role));
  // TODO: aider
  const deck = Array.from(DISTRICT_NAMES);
  shuffle(deck, asRng(prng));

  // Create the game state
  const game: GameState = {
    actions: [config],
    players,
    characters,
    deck,
    discard: [],
    prng,
    museum: { artifacts: [], cards: [] },
    logs: [],
    crowned: 0,
    taxCollector: 0,
    alchemist: 0,
    firstToComplete: null,
    followup: null,
    notifications: [],
    activeTurn: {
      type: "Draft",
      // TODO: aider
      draft: beginDraft(players.length, 0, ROLE_NAMES),
    },
  };
  // Deal initial cards
  for (const player of players) {
    player.hand = drawCards(game, 4);
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
  if (game.firstToComplete === null && player.city.length >= 8) {
    game.firstToComplete = playerIndex;
  }
}

export function discardDistrict(game: GameState, district: DistrictName): void {
  game.discard.push(district);
}

export function gainCards(game: GameState, amount: number): number {
  const player = getActivePlayer(game);
  if (!player) throw new Error();

  const cards = drawCards(game, amount);
  player.hand.push(...cards);
  return cards.length;
}

export function gainGoldForSuit(game: GameState, suit: CardSuit): ActionOutput {
  const player = getActivePlayer(game);
  if (!player) throw new Error("no active player");
  const amount = countSuitForResourceGain(player, suit);

  player.gold += amount;

  const activeRole = getActiveRole(game);
  return {
    log: `The ${activeRole} (${player.name}) gains ${amount} gold from their $${suit} districts.`,
  };
}

export function gainCardsForSuit(
  game: GameState,
  suit: CardSuit,
): ActionOutput {
  const player = getActivePlayer(game);
  if (!player) throw new Error("no active player");
  const count = countSuitForResourceGain(player, suit);

  const amount = gainCards(game, count);
  const activeRole = getActiveRole(game);
  return {
    log: `The ${activeRole} (${player.name}) gains ${amount} cards from their $${suit} districts.`,
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
