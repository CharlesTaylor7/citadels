import { Action, PlayerAction } from "./actions";
import { DistrictNameUtils } from "./districts";
import {
  ActionOutput,
  Followup,
  GameState,
  Notification,
  PlayerIndex,
  DistrictName,
} from "./game"; // Added PlayerIndex, DistrictName
import { RoleName, RoleNameUtils, RANKS } from "@/core/roles";

// Individual action handler functions
function handleRevealWarrant(
  game: GameState,
  _action: Extract<Action, { action: "RevealWarrant" }>,
): ActionOutput {
  let log = "";
  if (game.followup?.type !== "Warrant") {
    throw new Error("Invalid followup state for RevealWarrant");
  }
  const warrantFollowup = game.followup; // alias for type narrowing

  if (!warrantFollowup.signed) {
    const magistratePlayer = game.players[warrantFollowup.magistrate];
    if (!magistratePlayer) {
      throw new Error(
        `Invalid player index for Magistrate: ${warrantFollowup.magistrate}`,
      );
    }
    magistratePlayer.gold += warrantFollowup.gold;
    const districtData = DistrictNameUtils.data(warrantFollowup.district);

    if (game.activeTurn.type !== "Call") {
      throw new Error("RevealWarrant action must occur during a Call turn.");
    }
    const characterInTurn = game.characters[game.activeTurn.call.index];
    if (
      characterInTurn === undefined ||
      characterInTurn.playerIndex === undefined
    ) {
      throw new Error("No active player found for RevealWarrant logging.");
    }
    const activePlayer = game.players[characterInTurn.playerIndex];
    if (!activePlayer) {
      throw new Error("Active player not found.");
    }

    log = `${activePlayer.name} reveals an unsigned warrant. ${magistratePlayer.name} takes ${warrantFollowup.gold} gold and the ${districtData.displayName}.`;
  } else {
    if (game.activeTurn.type !== "Call") {
      throw new Error("RevealWarrant action must occur during a Call turn.");
    }
    const characterInTurn = game.characters[game.activeTurn.call.index];
    if (
      characterInTurn === undefined ||
      characterInTurn.playerIndex === undefined
    ) {
      throw new Error("No active player found for RevealWarrant.");
    }
    const activePlayerIndex = characterInTurn.playerIndex;
    const targetPlayer = game.players[activePlayerIndex];
    if (!targetPlayer) {
      throw new Error(`Invalid player index for target: ${activePlayerIndex}`);
    }

    const districtData = DistrictNameUtils.data(warrantFollowup.district);
    log = `${targetPlayer.name} reveals a signed warrant. The ${districtData.displayName} is confiscated from ${targetPlayer.name}.`;

    targetPlayer.city = targetPlayer.city.filter(
      (d) => d.name !== warrantFollowup.district,
    );
    game.deck.push(warrantFollowup.district);
  }
  game.followup = null; // Clear the followup
  return { log };
}

function handlePayBribe(
  game: GameState,
  _action: Extract<Action, { action: "PayBribe" }>,
): ActionOutput {
  const blackmailerRole = game.characters.find(
    (r) => r.role === "Blackmailer" && r.playerIndex !== undefined,
  );
  if (!blackmailerRole || blackmailerRole.playerIndex === undefined) {
    throw new Error("Blackmailer not found or has no assigned player.");
  }
  const blackmailerPlayerIndex = blackmailerRole.playerIndex;
  const blackmailerPlayer = game.players[blackmailerPlayerIndex];
  if (!blackmailerPlayer) {
    throw new Error(
      `Invalid player index for Blackmailer: ${blackmailerPlayerIndex}`,
    );
  }

  if (game.activeTurn.type !== "Call") {
    throw new Error("PayBribe action must occur during a Call turn.");
  }
  const characterInTurn = game.characters[game.activeTurn.call.index];
  if (
    characterInTurn === undefined ||
    characterInTurn.playerIndex === undefined
  ) {
    throw new Error("No active player found for PayBribe.");
  }
  const activePlayerIndex = characterInTurn.playerIndex;
  const activePlayer = game.players[activePlayerIndex];
  if (!activePlayer) {
    throw new Error("Active player not found.");
  }

  const bribeAmount = Math.floor(activePlayer.gold / 2);
  activePlayer.gold -= bribeAmount;
  blackmailerPlayer.gold += bribeAmount;

  const log = `${activePlayer.name} bribed the Blackmailer (${blackmailerPlayer.name}) with ${bribeAmount} gold.`;
  return { log };
}

function handleIgnoreBlackmail(
  game: GameState,
  _action: Extract<Action, { action: "IgnoreBlackmail" }>,
): ActionOutput {
  const blackmailerRole = game.characters.find(
    (r) => r.role === "Blackmailer" && r.playerIndex !== undefined,
  );
  if (!blackmailerRole || blackmailerRole.playerIndex === undefined) {
    // This case should ideally not happen if IgnoreBlackmail is a valid action,
    // as it implies a Blackmailer is in play and has targeted someone.
    throw new Error("Blackmailer not found or has no assigned player.");
  }
  const blackmailerPlayerIndex = blackmailerRole.playerIndex;

  // Determine active player name for logging
  if (game.activeTurn.type !== "Call") {
    throw new Error("IgnoreBlackmail action must occur during a Call turn.");
  }
  const characterInTurn = game.characters[game.activeTurn.call.index];
  if (
    characterInTurn === undefined ||
    characterInTurn.playerIndex === undefined
  ) {
    throw new Error("No active player found for IgnoreBlackmail logging.");
  }
  const activePlayer = game.players[characterInTurn.playerIndex];
  if (!activePlayer) {
    throw new Error("Active player not found.");
  }

  const log = `${activePlayer.name} ignored the blackmail. Waiting on the Blackmailer's response.`;
  const followup: Followup = {
    type: "Blackmail", // This should match the existing Followup type in game.ts
    blackmailer: blackmailerPlayerIndex,
  };

  return { log, followup };
}

// Helper to get the current character whose turn it is (based on Call phase)
function getCharacterInTurn(game: GameState): GameRole {
  if (game.activeTurn.type !== "Call") {
    // This check might be too restrictive if called by actions usable in other phases.
    // For now, assuming actions needing "active character" are in Call phase.
    throw new Error("Action requires to be in Call turn phase to identify active character.");
  }
  const character = game.characters[game.activeTurn.call.index];
  if (!character) {
    throw new Error(`Character not found at Call index ${game.activeTurn.call.index}`);
  }
  return character;
}

// Helper to get the player controlling the current character in turn
function getActivePlayer(game: GameState): Player {
  // If Draft phase, active player is directly from draft.playerIndex
  if (game.activeTurn.type === "Draft") {
    const player = game.players[game.activeTurn.draft.playerIndex];
    if (!player) {
      throw new Error(`Player not found at Draft index ${game.activeTurn.draft.playerIndex}`);
    }
    return player;
  }

  // For Call phase (and potentially others if active player logic is centralized)
  const characterInTurn = getCharacterInTurn(game);
  if (characterInTurn.playerIndex === undefined) {
    // TODO: Handle complex active player logic from Rust, e.g., Bewitched player's turn taken by Witch.
    // For now, this is a direct interpretation.
    throw new Error(`Character ${characterInTurn.role} has no assigned player for this turn.`);
  }
  const player = game.players[characterInTurn.playerIndex];
  if (!player) {
    throw new Error(`Player not found at index ${characterInTurn.playerIndex} for character ${characterInTurn.role}.`);
  }
  return player;
}

// Helper for after_gather_resources logic from Rust
function determineFollowupAfterGather(game: GameState): Followup | undefined {
  const characterInTurn = getCharacterInTurn(game);
  if (characterInTurn.markers.some(marker => marker.type === "Bewitched")) {
    return { type: "Bewitch" };
  }
  return undefined;
}

// Simplified helper for completing a build.
// TODO: Expand this to include all special district effects from Rust's `Game::complete_build`
// (e.g., PoorHouse, HauntedQuarter, Smithy, MapRoom, ImperialTreasury, SchoolOfMagic effects, cost interactions).
function completeBuild_simplified(
  game: GameState,
  playerIndex: PlayerIndex,
  districtName: DistrictName,
  _cost: number, // Cost is passed for potential future use (e.g. Smithy refund)
): void {
  const player = game.players[playerIndex];
  if (!player) {
    throw new Error(
      `Player not found at index ${playerIndex} for completeBuild`,
    );
  }

  // Check for duplicates if not Quarry or Wizard (simplified, actual build action would handle this better)
  // For now, Pass action assumes the build is valid if it reaches this stage.
  // if (player.city.some(d => d.name === districtName) && districtName !== "Quarry" /* and not wizard role */) {
  //   throw new Error(`Player ${player.name} already has district ${districtName} in their city.`);
  // }

  player.city.push({ name: districtName, beautified: false });

  // Simplified city size calculation (as in Player.city_size in Rust)
  const citySize = player.city.reduce(
    (acc, d) => acc + (d.name === "Monument" ? 2 : 1),
    0,
  );

  // Determine complete city size (as in Game.complete_city_size in Rust)
  const requiredCitySize = game.players.length <= 3 ? 8 : 7;

  if (citySize >= requiredCitySize && game.firstToComplete === null) {
    game.firstToComplete = playerIndex;
    // TODO: Consider adding a log or notification for "first to complete city".
  }
}

function handlePass(
  game: GameState,
  _action: Extract<Action, { action: "Pass" }>,
): ActionOutput {
  let log = "";

  if (!game.followup) {
    throw new Error(
      "Pass action is only valid when there is a followup pending.",
    );
  }

  const currentFollowup = game.followup;
  game.followup = null; // Clear followup immediately

  switch (currentFollowup.type) {
    case "Warrant": {
      // Active player is the Magistrate.
      // According to Rust, if Magistrate passes, they build the district.
      if (game.activeTurn.type !== "Call") {
        throw new Error("Pass (Warrant) action must occur during a Call turn.");
      }
      const characterInTurn = game.characters[game.activeTurn.call.index];
      if (
        characterInTurn === undefined ||
        characterInTurn.playerIndex === undefined
      ) {
        throw new Error("No active player (Magistrate) found for Pass action.");
      }
      const magistratePlayerIndex = characterInTurn.playerIndex;
      const magistratePlayer = game.players[magistratePlayerIndex];
      if (!magistratePlayer) {
        throw new Error("Magistrate player object not found.");
      }

      // The magistrate from the followup is the same as the active player here.
      const magistrateName =
        game.players[currentFollowup.magistrate]?.name ?? "Unknown Magistrate";

      completeBuild_simplified(
        game,
        magistratePlayerIndex,
        currentFollowup.district,
        currentFollowup.gold, // This is the original cost
      );
      log = `The Magistrate (${magistrateName}) did not reveal the warrant. The ${currentFollowup.district} is built.`;
      // NOTE: This interpretation (Magistrate builds it) is based on a direct reading of Rust's
      // `game.complete_build(game.active_player_index()?, gold, district);` where active_player is Magistrate.
      // This might need review as it's counter-intuitive that the original builder loses out.
      break;
    }
    case "Blackmail": {
      // Active player is the Blackmailer.
      if (game.activeTurn.type !== "Call") {
        throw new Error(
          "Pass (Blackmail) action must occur during a Call turn.",
        );
      }
      const characterInTurn = game.characters[game.activeTurn.call.index];
      if (
        characterInTurn === undefined ||
        characterInTurn.playerIndex === undefined
      ) {
        throw new Error(
          "No active player (Blackmailer) found for Pass action.",
        );
      }
      // const blackmailerPlayerIndex = characterInTurn.playerIndex;
      const blackmailerPlayer = game.players[currentFollowup.blackmailer];
      if (!blackmailerPlayer) {
        throw new Error("Blackmailer player object not found from followup.");
      }
      log = `The Blackmailer (${blackmailerPlayer.name}) did not reveal the blackmail.`;
      break;
    }
    default:
      // Restore followup if it was not a type we handle with Pass
      game.followup = currentFollowup;
      throw new Error(
        `Pass action is not applicable for the current followup type: ${
          currentFollowup.type
        }`,
      );
  }

  return { log };
}

function handleRevealBlackmail(
  game: GameState,
  _action: Extract<Action, { action: "RevealBlackmail" }>,
): ActionOutput {
  if (game.followup?.type !== "Blackmail") {
    throw new Error("Invalid followup state for RevealBlackmail");
  }
  const blackmailFollowup = game.followup;
  const blackmailerPlayerIndex = blackmailFollowup.blackmailer;
  const blackmailerPlayer = game.players[blackmailerPlayerIndex];

  if (!blackmailerPlayer) {
    throw new Error(
      `Blackmailer player not found at index ${blackmailerPlayerIndex}`,
    );
  }

  if (game.activeTurn.type !== "Call") {
    throw new Error("RevealBlackmail action must occur during a Call turn.");
  }
  const characterInTurn = game.characters[game.activeTurn.call.index];
  if (
    characterInTurn === undefined ||
    characterInTurn.playerIndex === undefined
  ) {
    throw new Error("No active character/player found for RevealBlackmail.");
  }
  const activePlayerIndex = characterInTurn.playerIndex;
  const activePlayer = game.players[activePlayerIndex];
  if (!activePlayer) {
    throw new Error("Active player (target of blackmail) not found.");
  }

  const isFlowered = characterInTurn.markers.some(
    (marker) => marker.type === "Blackmail" && marker.flowered,
  );

  let log = "";

  if (isFlowered) {
    const goldTaken = activePlayer.gold;
    activePlayer.gold = 0;
    blackmailerPlayer.gold += goldTaken;

    // Clear all blackmail markers from all characters
    game.characters.forEach((character) => {
      character.markers = character.markers.filter(
        (marker) => marker.type !== "Blackmail",
      );
    });

    log = `The Blackmailer (${blackmailerPlayer.name}) reveals an active threat against ${activePlayer.name}, and takes all ${goldTaken} of their gold.`;
  } else {
    log = `The Blackmailer (${blackmailerPlayer.name}) reveals an empty threat against ${activePlayer.name}. Nothing happens.`;
  }

  game.followup = null; // Clear the followup
  return { log };
}

function handleDraftPick(
  game: GameState,
  action: Extract<Action, { action: "DraftPick" }>,
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
  action: Extract<Action, { action: "DraftDiscard" }>,
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
  _action: Extract<Action, { action: "GatherResourceCards" }>,
): ActionOutput {
  const activePlayer = getActivePlayer(game);

  let drawAmount = 2;
  if (activePlayer.city.some(d => d.name === "Observatory")) {
    drawAmount += 1;
  }

  // Draw cards from the deck (removes them from game.deck)
  const drawnCards = game.deck.splice(0, drawAmount);

  let log = "";
  let finalFollowup: Followup | undefined = undefined;

  if (activePlayer.city.some(d => d.name === "Library")) {
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
  _action: Extract<Action, { action: "GatherResourceGold" }>,
): ActionOutput {
  const activePlayer = getActivePlayer(game);
  let amount = 2;
  let log = "";

  if (activePlayer.city.some(d => d.name === "GoldMine")) {
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
  action: Extract<Action, { action: "Assassinate" }>,
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
  targetCharacter.markers.push("Killed");

  const assassinPlayer = getActivePlayer(game); // Assumes Assassin is the active player

  const log = `The Assassin (${assassinPlayer.name}) kills the ${RoleNameUtils.data(targetRoleName).name}; Their turn will be skipped.`;

  return { log };
}

function handleSteal(
  game: GameState,
  action: Extract<Action, { action: "Steal" }>,
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
      (marker) => marker === "Killed" || marker.type === "Bewitched",
    )
  ) {
    throw new Error(
      `Cannot rob from the ${RoleNameUtils.data(targetRoleName).name} who is killed or bewitched.`,
    );
  }

  // Ensure the "Robbed" marker is not already present, though Rust code doesn't check this.
  // It's generally safe to add.
  targetCharacter.markers.push("Robbed");

  const thiefPlayer = getActivePlayer(game); // Assumes Thief is the active player

  const log = `The Thief (${thiefPlayer.name}) robs the ${RoleNameUtils.data(targetRoleName).name}; At the start of their turn, all their gold will be taken.`;

  return { log };
}

// Map of action types to their handlers
// We use `Extract<Action, { action: K }>` to ensure type safety for each handler's action parameter.
// The `as any` cast is a pragmatic way to satisfy TypeScript's complex type inference for such dynamic dispatch maps.
export const playerActionHandlers: {
  [K in PlayerAction["action"]]: (
    game: GameState,
    action: Extract<PlayerAction, { action: K }>,
  ) => ActionOutput;
} = {
  RevealWarrant: handleRevealWarrant,
  PayBribe: handlePayBribe,
  IgnoreBlackmail: handleIgnoreBlackmail,
  Pass: handlePass,
  RevealBlackmail: handleRevealBlackmail,
  DraftPick: handleDraftPick,
  DraftDiscard: handleDraftDiscard,
  GatherResourceCards: handleGatherResourceCards,
  GatherResourceGold: handleGatherResourceGold,
  Assassinate: handleAssassinate,
  Steal: handleSteal,
  // Other PlayerAction handlers will be added here
};
