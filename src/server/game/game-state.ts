import { PlayerAction } from "@/core/actions";
import {
  DistrictName,
  DISTRICT_NAMES,
  DistrictNameUtils,
} from "@/core/districts";
import { RoleName, ROLE_NAMES } from "@/core/roles";
import { CardSuit, PlayerId } from "@/core/types";
import { GameConfig } from "@/core/config";
import { Museum } from "@/server/game/museum";
import { asRng, newPrng, PRNG, shuffle } from "@/server/game/random";

/*
#[derive(Debug)]
pub enum Followup {
    Bewitch,
    GatherCardsPick {
        revealed: Vec<DistrictName>,
    },
    ScholarPick {
        revealed: Vec<DistrictName>,
    },
    WizardPick {
        player: PlayerIndex,
    },
    SeerDistribute {
        players: Vec<PlayerIndex>,
    },
    SpyAcknowledge {
        player: PlayerName,
        revealed: Vec<DistrictName>,
    },
    Warrant {
        signed: bool,
        magistrate: PlayerIndex,
        gold: usize,
        district: DistrictName,
    },
    Blackmail {
        blackmailer: PlayerIndex,
    },
    HandleBlackmail,
}
*/
export interface Followup {}

export interface ActionOutput {
  log: string;
  followup: Followup | null;
  end_turn: boolean;
  notifications: Notification[];
}

export type PlayerIndex = number;

/**
 * Player in the game
 */
export interface Player {
  index: PlayerIndex;
  id: PlayerId;
  name: string;
  gold: number;
  hand: DistrictName[];
  city: CityDistrict[];
  roles: RoleName[];
}

/**
 * District in a player's city
 */
export interface CityDistrict {
  name: DistrictName;
  beautified: boolean;
}

/**
 * Character in the game
 */
export interface Character {
  role: RoleName;
  player: PlayerIndex | null;
  markers: (string | object)[];
  logs: string[];
  actions: PlayerAction[];
}

export interface Characters {
  characters: Character[];
}

export interface Draft {
  playerCount: number;
  player: PlayerIndex;
  remaining: RoleName[];
  theaterStep: boolean;
  initialDiscard: RoleName | null;
  faceupDiscard: RoleName[];
}

export interface Call {
  index: number;
  endOfRound: boolean;
}

export type TurnType = "GameOver" | "Draft" | "Call";

export type Turn =
  | { type: "GameOver" }
  | { type: "Draft"; draft: Draft }
  | { type: "Call"; call: Call };

export interface Notification {
  message: string;
  playerIndex: PlayerIndex | null;
}

export interface Deck<T> {
  deck: T[];
  discard: T[];
  prng: PRNG;
}

export interface GameState {
  players: Player[];
  characters: Characters;
  deck: Deck<DistrictName>;
  logs: string[];
  activeTurn: Turn;
  crowned: PlayerIndex;
  firstToComplete: PlayerIndex | null;
  followup: Followup | null;
  notifications: Notification[];
  museum: Museum;
  alchemist: number;
  taxCollector: number;
}

export function createPlayer(
  index: PlayerIndex,
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

export function createCharacter(role: RoleName): Character {
  return {
    role,
    player: null,
    markers: [],
    logs: [],
    actions: [],
  };
}

export function createCharacters(roles: readonly RoleName[]): Characters {
  return {
    characters: roles.map((role) => createCharacter(role)),
  };
}

export function createDeck<T>(items: readonly T[], prng: PRNG): Deck<T> {
  return {
    deck: [...items],
    discard: [],
    prng,
  };
}

export function shuffleDeck<T>(deck: Deck<T>): void {
  deck.deck.push(...deck.discard);
  deck.discard = [];
  shuffle(deck.deck, asRng(deck.prng));
}

/**
 * Draw a card from the deck
 */
export function drawCard<T>(deck: Deck<T>): T | undefined {
  if (deck.deck.length === 0) {
    if (deck.discard.length === 0) {
      return undefined;
    }
    shuffleDeck(deck);
  }
  return deck.deck.pop();
}

/**
 * Draw multiple cards from the deck
 */
export function drawCards<T>(deck: Deck<T>, count: number): T[] {
  const cards: T[] = [];
  for (let i = 0; i < count; i++) {
    const card = drawCard(deck);
    if (card !== undefined) {
      cards.push(card);
    }
  }
  return cards;
}

/**
 * Discard a card to the bottom of the deck
 */
export function discardToBottom<T>(deck: Deck<T>, card: T): void {
  deck.discard.unshift(card);
}

/**
 * Create a new draft
 */
export function beginDraft(
  playerCount: number,
  player: PlayerIndex,
  roles: readonly RoleName[],
): Draft {
  const draft: Draft = {
    playerCount,
    player,
    remaining: [...roles],
    theaterStep: false,
    initialDiscard: null,
    faceupDiscard: [],
  };

  const roleCount = draft.remaining.length;

  // Discard cards face up in 4+ player game
  if (draft.playerCount >= 4) {
    for (let i = 0; i < draft.playerCount + 2 - roleCount; i++) {
      // Find a card that can be discarded
      let index = 0;
      let canBeDiscarded = false;

      while (!canBeDiscarded && index < draft.remaining.length) {
        // This is a placeholder - implement the actual logic
        canBeDiscarded = true;
        index = Math.floor(Math.random() * draft.remaining.length);
      }

      const removed = draft.remaining.splice(index, 1)[0];
      draft.faceupDiscard.push(removed);
    }
  }

  // Discard 1 card facedown
  const i = Math.floor(Math.random() * draft.remaining.length);
  draft.initialDiscard = draft.remaining.splice(i, 1)[0];

  // Restore sort of roles
  draft.remaining.sort((a, b) => {
    const rankA = ROLE_NAMES.indexOf(a);
    const rankB = ROLE_NAMES.indexOf(b);
    return rankA - rankB;
  });

  return draft;
}

/**
 * Create a new game state
 */
export function createGame(config: GameConfig): GameState {
  // Initialize players
  const players = config.players.map((player, index) =>
    createPlayer(index, player.id, player.name),
  );
  const prng = newPrng(config.rngSeed);

  // Initialize characters
  const characters = createCharacters(ROLE_NAMES);

  // Initialize deck with all district cards
  const deck = createDeck<DistrictName>(DISTRICT_NAMES, prng);
  shuffleDeck(deck);

  // Initialize museum
  const museum = new Museum();

  // Deal initial cards
  for (const player of players) {
    player.hand = drawCards(deck, 4);
  }

  // Create the game state
  return {
    players,
    characters,
    deck,
    museum,
    logs: [],
    crowned: 0,
    taxCollector: 0,
    alchemist: 0,
    firstToComplete: null,
    followup: null,
    notifications: [],
    activeTurn: {
      type: "Draft",
      draft: beginDraft(players.length, 0, ROLE_NAMES),
    },
  };
}

/**
 * Get the active player
 */
export function getActivePlayer(game: GameState): Player | undefined {
  const activeRole = getActiveRole(game);
  if (!activeRole || activeRole.player === null) {
    return undefined;
  }
  return game.players[activeRole.player];
}

/**
 * Get the active role
 */
export function getActiveRole(game: GameState): Character | undefined {
  if (game.activeTurn.type === "Call") {
    return game.characters.characters[game.activeTurn.call.index];
  }
  return undefined;
}

/**
 * Complete a build action
 */
export function completeAction(
  game: GameState,
  playerIndex: PlayerIndex,
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

/**
 * Discard a district
 */
export function discardDistrict(game: GameState, district: DistrictName): void {
  discardToBottom(game.deck, district);
}

/**
 * Gain cards for the active player
 */
export function gainCards(game: GameState, amount: number): void {
  const player = getActivePlayer(game);
  if (!player) return;

  const cards = drawCards(game.deck, amount);
  player.hand.push(...cards);

  const activeRole = getActiveRole(game);
  if (activeRole) {
    activeRole.logs.push(`${player.name} gains ${amount} cards.`);
  }
}

/**
 * Gain resources for districts of a specific suit
 */
export function gainResourcesForSuit(game: GameState, suit: CardSuit): void {
  const player = getActivePlayer(game);
  if (!player) return;

  const count = player.city.filter(
    (c) =>
      c.name === "SchoolOfMagic" ||
      DistrictNameUtils.data(c.name).suit === suit,
  ).length;

  if (count > 0) {
    player.gold += count;

    const activeRole = getActiveRole(game);
    if (activeRole) {
      activeRole.logs.push(
        `${player.name} gains ${count} gold from ${suit} districts.`,
      );
    }
  }
}

/**
 * Begin the draft phase
 */
export function beginGameDraft(game: GameState): void {
  game.activeTurn = {
    type: "Draft",
    draft: beginDraft(game.players.length, game.crowned, ROLE_NAMES),
  };
}

/**
 * Move to the next character in the call phase
 */
export function callNext(game: GameState): void {
  if (game.activeTurn.type !== "Call") return;

  const call = game.activeTurn.call;
  // Find the next character with a player
  let nextIndex = call.index + 1;
  while (
    nextIndex < game.characters.characters.length &&
    game.characters.characters[nextIndex].player === null
  ) {
    nextIndex++;
  }

  if (nextIndex < game.characters.characters.length) {
    game.activeTurn = {
      type: "Call",
      call: {
        index: nextIndex,
        endOfRound: false,
      },
    };
  } else {
    // Check for Emperor
    const emperor = game.characters.characters.findIndex(
      (c) => c.role === "Emperor",
    );
    if (emperor >= 0) {
      game.activeTurn = {
        type: "Call",
        call: {
          index: emperor,
          endOfRound: true,
        },
      };
    } else {
      endRound(game);
    }
  }
}

/**
 * Clean up the round
 */
export function cleanupRound(game: GameState): void {
  // Clean up characters
  for (const character of game.characters.characters) {
    character.player = null;
    character.markers = [];
    character.logs = [];
    character.actions = [];
  }

  // Clean up players
  for (const player of game.players) {
    player.roles = [];
  }
}

/**
 * End the current round
 */
export function endRound(game: GameState): void {
  // Check for heir (King or Patrician that was killed)
  const heir = game.characters.characters.find(
    (c) =>
      c.markers.includes("Killed") &&
      (c.role === "King" || c.role === "Patrician"),
  );

  if (heir && heir.player) {
    game.crowned = heir.player;
    game.logs.push(
      `${heir.role}'s heir ${game.players[game.crowned].name} crowned.`,
    );
  }

  // Check for game over
  if (game.firstToComplete !== null) {
    game.activeTurn = { type: "GameOver" };
    return;
  }

  // Clean up the round
  cleanupRound(game);

  // Begin next draft
  beginGameDraft(game);
}

/**
 * Start a new turn
 */
export function startTurn(game: GameState) {
  switch (game.activeTurn.type) {
    case "GameOver":
      return { success: true, value: undefined };

    case "Draft": {
      const draft = game.activeTurn.draft;
      if (draft.theaterStep) {
        // Start call phase
        game.activeTurn = {
          type: "Call",
          call: {
            index: 0,
            endOfRound: false,
          },
        };
      } else {
        // Handle special cases for different player counts
        if (
          game.players.length === 3 &&
          game.characters.characters.length === 9 &&
          draft.remaining.length === 5
        ) {
          // For 3 player game with 9 characters, discard 1 randomly
          const index = Math.floor(Math.random() * draft.remaining.length);
          draft.remaining.splice(index, 1);
        }
      }
      break;
    }

    case "Call": {
      const call = game.activeTurn.call;
      const character = game.characters.characters[call.index];

      // If character has a player, add actions
      if (character.player !== null) {
        const player = game.players[character.player];

        // Add standard actions
        character.actions = [
          { action: "GatherResourceGold" },
          { action: "GatherResourceCards" },
        ];

        // Add character-specific actions
        switch (character.role) {
          case "Assassin":
            // Add assassin actions
            break;

          case "Thief":
            // Add thief actions
            break;

          case "Magician":
            // Add magician actions
            break;

          case "King":
            // Add king actions
            // King gets resources for noble districts and is crowned
            game.crowned = character.player;
            gainResourcesForSuit(game, "Noble");
            break;

          case "Bishop":
            // Add bishop actions
            // Bishop gets resources for religious districts
            gainResourcesForSuit(game, "Religious");
            break;

          case "Merchant":
            // Add merchant actions
            // Merchant gets 1 gold and resources for trade districts
            player.gold += 1;
            character.logs.push(`${player.name} gains 1 gold as the Merchant.`);
            gainResourcesForSuit(game, "Trade");
            break;

          case "Architect":
            // Add architect actions
            // Architect gets 2 cards
            gainCards(game, 2);
            break;

          case "Warlord":
            // Add warlord actions
            // Warlord gets resources for military districts
            gainResourcesForSuit(game, "Military");
            break;

          default:
            // No special actions for other characters
            break;
        }
      }
      break;
    }
  }

  return { success: true, value: undefined };
}

/**
 * Process the end of a turn
 */
export function processTurnEnd(game: GameState) {
  switch (game.activeTurn.type) {
    case "GameOver":
      break;

    case "Draft": {
      const draft = game.activeTurn.draft;
      if (draft.theaterStep) {
        // Move to call phase
        game.activeTurn = {
          type: "Call",
          call: {
            index: 0,
            endOfRound: false,
          },
        };
      } else {
        // Move to next player in draft
        draft.player = (draft.player + 1) % game.players.length;
      }
      break;
    }

    case "Call": {
      const call = game.activeTurn.call;
      const activeRole = game.characters.characters[call.index];

      // If this is the end of the round, end it
      if (call.endOfRound) {
        endRound(game);
      } else {
        // Handle Alchemist refund
        const refund = game.alchemist;
        if (refund > 0 && activeRole.player) {
          game.alchemist = 0;
          const player = game.players[activeRole.player];
          player.gold += refund;
          activeRole.logs.push(
            `The Alchemist is refunded ${refund} gold spent building.`,
          );
        }

        callNext(game);
      }
      break;
    }
  }

  return startTurn(game);
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
