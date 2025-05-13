import {
  Action,
  PlayerAction,
} from "./actions";
import { DistrictNameUtils } from "./districts";
import { ActionOutput, Followup, GameState, Notification } from "./game"; // Assuming ActionOutput, GameState etc. are exported from game.ts

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
      throw new Error(
        `Invalid player index for target: ${activePlayerIndex}`,
      );
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

// Map of action types to their handlers
// We use `Extract<Action, { action: K }>` to ensure type safety for each handler's action parameter.
// The `as any` cast is a pragmatic way to satisfy TypeScript's complex type inference for such dynamic dispatch maps.
export const playerActionHandlers: {
  [K in PlayerAction["action"]]?: (
    game: GameState,
    action: Extract<PlayerAction, { action: K }>,
  ) => ActionOutput;
} = {
  RevealWarrant: handleRevealWarrant as any,
  PayBribe: handlePayBribe as any,
  IgnoreBlackmail: handleIgnoreBlackmail as any,
  // Other PlayerAction handlers will be added here
};
