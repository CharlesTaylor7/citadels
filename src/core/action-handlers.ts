import { ActionTag, PlayerAction } from "./actions";
import { DistrictNameUtils } from "./districts";
import {
  ActionOutput,
  Followup,
  GameState,
  Player,
  GameRole,
} from "@/core/game";
import { RoleName, RoleNameUtils, RANKS } from "@/core/roles";
import { CardSuit } from "@/core/types";

// Individual action handler functions

// Helper to get the current character whose turn it is (based on Call phase)
function getCharacterInTurn(game: GameState): GameRole {
  if (game.activeTurn.type !== "Call") {
    // This check might be too restrictive if called by actions usable in other phases.
    // For now, assuming actions needing "active character" are in Call phase.
    throw new Error(
      "Action requires to be in Call turn phase to identify active character.",
    );
  }
  const character = game.characters[game.activeTurn.call.index];
  if (!character) {
    throw new Error(
      `Character not found at Call index ${game.activeTurn.call.index}`,
    );
  }
  return character;
}

// Helper to get the player controlling the current character in turn
function getActivePlayer(game: GameState): Player {
  // If Draft phase, active player is directly from draft.playerIndex
  if (game.activeTurn.type === "Draft") {
    const player = game.players[game.activeTurn.draft.playerIndex];
    if (!player) {
      throw new Error(
        `Player not found at Draft index ${game.activeTurn.draft.playerIndex}`,
      );
    }
    return player;
  }

  // For Call phase (and potentially others if active player logic is centralized)
  const characterInTurn = getCharacterInTurn(game);
  if (characterInTurn.playerIndex === undefined) {
    // TODO: Handle complex active player logic from Rust, e.g., Bewitched player's turn taken by Witch.
    // For now, this is a direct interpretation.
    throw new Error(
      `Character ${characterInTurn.role} has no assigned player for this turn.`,
    );
  }
  const player = game.players[characterInTurn.playerIndex];
  if (!player) {
    throw new Error(
      `Player not found at index ${characterInTurn.playerIndex} for character ${characterInTurn.role}.`,
    );
  }
  return player;
}

// Helper for after_gather_resources logic from Rust
function determineFollowupAfterGather(game: GameState): Followup | undefined {
  const characterInTurn = getCharacterInTurn(game);
  if (characterInTurn.markers.some((marker) => marker.tag === "Bewitched")) {
    return { type: "Bewitch" };
  }
  return undefined;
}

function handleDraftPick(
  game: GameState,
  action: ActionCase<"DraftPick">,
): ActionOutput {
  if (game.activeTurn.type !== "Draft") {
    throw new Error("DraftPick action is only valid during a Draft turn.");
  }
  const draft = game.activeTurn.draft;
  const pickedRole = action.role;

  // Remove pickedRole from draft.remaining
  const roleIndexInRemaining = draft.remaining.indexOf(pickedRole);
  if (roleIndexInRemaining === -1) {
    throw new Error(
      `Role ${pickedRole} not found in remaining draft roles. Available: ${draft.remaining.join(", ")}`,
    );
  }
  draft.remaining.splice(roleIndexInRemaining, 1);

  // Assign player to the character
  const character = game.characters.find((c) => c.role === pickedRole);
  if (!character) {
    // This should not happen if roles are set up correctly
    throw new Error(
      `Character for role ${pickedRole} not found in game characters.`,
    );
  }
  character.playerIndex = draft.playerIndex;

  // Add role to player's roles and sort
  const player = game.players[draft.playerIndex];
  if (!player) {
    throw new Error(`Player not found at index ${draft.playerIndex}.`);
  }
  player.roles.push(pickedRole);
  player.roles.sort(
    (a, b) =>
      RANKS.indexOf(RoleNameUtils.data(a).rank) -
      RANKS.indexOf(RoleNameUtils.data(b).rank),
  );

  const log = `${player.name} drafts a role.`;

  let endTurn = true;
  // In a 2-player game, the turn does not end after certain picks to allow for a discard.
  // Rust: if game.players.len() == 2 && (draft.remaining.len() == 5 || draft.remaining.len() == 3)
  // This means if the condition is true, end_turn is false. Otherwise, it's true.
  if (
    game.players.length === 2 &&
    (draft.remaining.length === 5 || draft.remaining.length === 3)
  ) {
    endTurn = false;
  }

  return { log, end_turn: endTurn };
}

function handleDraftDiscard(
  game: GameState,
  action: ActionCase<"DraftDiscard">,
): ActionOutput {
  if (game.activeTurn.type !== "Draft") {
    throw new Error("DraftDiscard action is only valid during a Draft turn.");
  }
  const draft = game.activeTurn.draft;
  const roleToDiscard = action.role;

  const roleIndexInRemaining = draft.remaining.indexOf(roleToDiscard);
  if (roleIndexInRemaining === -1) {
    throw new Error(
      `Role ${roleToDiscard} not found in remaining draft roles to discard. Available: ${draft.remaining.join(", ")}`,
    );
  }
  draft.remaining.splice(roleIndexInRemaining, 1);

  const activePlayer = game.players[draft.playerIndex];
  if (!activePlayer) {
    throw new Error(`Active player not found at index ${draft.playerIndex}.`);
  }

  const log = `${activePlayer.name} discards a role face down.`;
  return { log, end_turn: true };
}

function handleGatherResourceCards(
  game: GameState,
  _action: ActionCase<"GatherResourceCards">,
): ActionOutput {
  const activePlayer = getActivePlayer(game);

  let drawAmount = 2;
  if (activePlayer.city.some((d) => d.name === "Observatory")) {
    drawAmount += 1;
  }

  // Draw cards from the deck (removes them from game.deck)
  const drawnCards = game.deck.splice(0, drawAmount);

  let log = "";
  let finalFollowup: Followup | undefined = undefined;

  if (activePlayer.city.some((d) => d.name === "Library")) {
    activePlayer.hand.push(...drawnCards);
    log = `${activePlayer.name} gathers cards. With the aid of their library they keep all ${drawnCards.length} cards.`;
    finalFollowup = determineFollowupAfterGather(game);
  } else {
    log = `${activePlayer.name} reveals ${drawnCards.length} cards from the top of the deck.`;
    if (drawnCards.length > 0) {
      finalFollowup = { type: "GatherCardsPick", revealed: drawnCards };
    } else {
      // If no cards drawn (e.g., deck was empty), then check for Bewitch followup
      finalFollowup = determineFollowupAfterGather(game);
    }
  }

  return { log, followup: finalFollowup };
}

function handleGatherResourceGold(
  game: GameState,
  _action: ActionCase<"GatherResourceGold">,
): ActionOutput {
  const activePlayer = getActivePlayer(game);
  let amount = 2;
  let log = "";

  if (activePlayer.city.some((d) => d.name === "GoldMine")) {
    amount += 1;
    log = `${activePlayer.name} gathers 3 gold. (1 extra from their Gold Mine).`;
  } else {
    log = `${activePlayer.name} gathers 2 gold.`;
  }

  activePlayer.gold += amount;
  const followup = determineFollowupAfterGather(game);

  return { log, followup };
}

function handleAssassinate(
  game: GameState,
  action: ActionCase<"Assassinate">,
): ActionOutput {
  const targetRoleName = action.role;

  if (targetRoleName === "Assassin") {
    throw new Error("Cannot assassinate self (Assassin).");
  }

  const targetCharacter = game.characters.find(
    (c) => c.role === targetRoleName,
  );

  if (!targetCharacter) {
    throw new Error(
      `Role ${targetRoleName} not found in this game's characters.`,
    );
  }

  // Ensure the "Killed" marker is not already present, though Rust code doesn't check this.
  // It's generally safe to add, as multiple "Killed" markers don't change behavior.
  targetCharacter.markers.push({ tag: "Killed" });

  const assassinPlayer = getActivePlayer(game); // Assumes Assassin is the active player

  const log = `The Assassin (${assassinPlayer.name}) kills the ${RoleNameUtils.data(targetRoleName).name}; Their turn will be skipped.`;

  return { log };
}

function handleSteal(
  game: GameState,
  action: ActionCase<"Steal">,
): ActionOutput {
  const targetRoleName = action.role;

  if (targetRoleName === "Thief") {
    throw new Error("Cannot steal from self (Thief).");
  }

  const targetCharacter = game.characters.find(
    (c) => c.role === targetRoleName,
  );

  if (!targetCharacter) {
    throw new Error(
      `Role ${targetRoleName} not found in this game's characters.`,
    );
  }

  if (targetCharacter.revealed) {
    throw new Error(
      `Cannot steal from ${RoleNameUtils.data(targetRoleName).name} who has already taken their turn.`,
    );
  }

  if (
    targetCharacter.markers.some(
      (marker) => marker.tag === "Killed" || marker.tag === "Bewitched",
    )
  ) {
    throw new Error(
      `Cannot rob from the ${RoleNameUtils.data(targetRoleName).name} who is killed or bewitched.`,
    );
  }

  // Ensure the "Robbed" marker is not already present, though Rust code doesn't check this.
  // It's generally safe to add.
  targetCharacter.markers.push({ tag: "Robbed" });

  const thiefPlayer = getActivePlayer(game); // Assumes Thief is the active player

  const log = `The Thief (${thiefPlayer.name}) robs the ${RoleNameUtils.data(targetRoleName).name}; At the start of their turn, all their gold will be taken.`;

  return { log };
}

function handleTakeCrown(
  game: GameState,
  _action: ActionCase<"TakeCrown">,
): ActionOutput {
  // Find King or Patrician
  const royalCharacter = game.characters.find(
    (c) =>
      (c.role === "King" || c.role === "Patrician") &&
      c.playerIndex !== undefined,
  );

  if (!royalCharacter || royalCharacter.playerIndex === undefined) {
    // This check is a bit redundant due to the find condition, but good for type safety
    throw new Error("No King or Patrician found to take the crown.");
  }

  const royalPlayerIndex = royalCharacter.playerIndex;
  game.crowned = royalPlayerIndex;
  const royalPlayer = game.players[royalPlayerIndex];

  if (!royalPlayer) {
    throw new Error(
      `Player not found for crowned role ${royalCharacter.role} at index ${royalPlayerIndex}.`,
    );
  }

  const log = `${royalPlayer.name} takes the crown.`;
  return { log };
}

// Helper function to count districts of a specific suit for resource gain,
// mirroring Rust's `Player::count_suit_for_resource_gain`.
function countSuitForResourceGain(player: Player, suit: CardSuit): number {
  return player.city.filter(
    (cityDistrict) =>
      DistrictNameUtils.data(cityDistrict.name).suit === suit ||
      cityDistrict.name === "SchoolOfMagic",
  ).length;
}

// Generic handler for gaining gold based on suit count.
function handleGainGoldForSuit(
  game: GameState,
  suit: CardSuit,
  roleForLog: RoleName, // e.g., "King" for Noble, "Bishop" for Religious
): ActionOutput {
  const activePlayer = getActivePlayer(game);
  const count = countSuitForResourceGain(activePlayer, suit);
  activePlayer.gold += count;

  const log = `The ${RoleNameUtils.data(roleForLog).name} (${activePlayer.name}) gains ${count} gold from their ${suit} districts.`;
  return { log };
}

function handleGoldFromReligion(
  game: GameState,
  _action: ActionCase<"GoldFromReligion">,
): ActionOutput {
  // Assuming Bishop is the role associated with gaining gold from Religious districts for logging.
  // This might need adjustment if other roles can trigger this or if the log should be more generic.
  const activeCharacter = getCharacterInTurn(game);
  return handleGainGoldForSuit(game, "Religious", activeCharacter.role);
}

// @ts-expect-error Not all handlers are implemented
const playerActionHandlers: {
  [K in ActionTag]: (game: GameState, action: ActionCase<K>) => ActionOutput;
} = {
  DraftPick: handleDraftPick,
  DraftDiscard: handleDraftDiscard,
  GatherResourceCards: handleGatherResourceCards,
  GatherResourceGold: handleGatherResourceGold,
  TakeCrown: handleTakeCrown,
};
type ActionCase<K> = Extract<PlayerAction, { tag: K }>;

export function runHandler(
  game: GameState,
  action: PlayerAction,
): ActionOutput {
  const handler = playerActionHandlers[action.tag];
  // @ts-expect-error The handler dictionary ensures the type is right. This line gets confused because TS doesn;t do existential types. It fails to infer that the action matches the handler.
  return handler(game, action);
}
