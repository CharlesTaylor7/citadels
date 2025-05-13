import { Marker, PlayerId } from "@/core/types";
import { RoleName } from "@/core/roles";
import { DistrictName, DistrictNameUtils } from "@/core/districts";
import { PRNG } from "@/server/game/random";
import { Action } from "./actions";

export interface GameState {
  players: Player[];
  characters: GameRole[];
  deck: DistrictName[];
  discard: DistrictName[];
  logs: string[];
  activeTurn: Turn;
  crowned: PlayerIndex;
  firstToComplete: PlayerIndex | null;
  followup: Followup | null;
  notifications: Notification[];
  museum: Museum;
  alchemist: number;
  taxCollector: number;
  prng: PRNG;
  // record of all game actions
  // whole state should be recreatable by replaying this log
  actions: Action[];
}
export type Followup =
  | { type: "Bewitch" }
  | { type: "GatherCardsPick"; revealed: DistrictName[] }
  | { type: "ScholarPick"; revealed: DistrictName[] }
  | { type: "WizardPick"; player: PlayerIndex }
  | { type: "SeerDistribute"; players: PlayerIndex[] }
  | { type: "SpyAcknowledge"; player: PlayerIndex; revealed: DistrictName[] }
  | {
      type: "Warrant";
      signed: boolean;
      magistrate: PlayerIndex;
      gold: number;
      district: DistrictName;
    }
  | { type: "Blackmail"; blackmailer: PlayerIndex }
  | { type: "HandleBlackmail" };

export interface ActionOutput {
  log: string;
  followup?: Followup;
  end_turn?: boolean;
  notifications?: Notification[];
}

export type PlayerIndex = number;

export interface Player {
  index: PlayerIndex;
  id: PlayerId;
  name: string;
  gold: number;
  hand: DistrictName[];
  city: CityDistrict[];
  roles: RoleName[];
}

export interface CityDistrict {
  name: DistrictName;
  beautified: boolean;
}

export interface GameRole {
  role: RoleName;
  markers: Marker[];
  playerIndex?: PlayerIndex;
  revealed: boolean;
  logs: string[];
}

export interface Draft {
  playerCount: number;
  playerIndex: PlayerIndex;
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

export interface Museum {
  cards: DistrictName[];
  artifacts: string[];
}

export function performAction(game: GameState, action: Action): ActionOutput {
  let log = "";
  let notifications: Notification[] | undefined = undefined;
  let followup: Followup | null = null; // By default, followup is cleared unless set otherwise

  switch (action.action) {
    case "RevealWarrant": {
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

        // Determine active player name for logging
        if (game.activeTurn.type !== "Call") {
            throw new Error("RevealWarrant action must occur during a Call turn.");
        }
        const characterInTurn = game.characters[game.activeTurn.call.index];
        if (characterInTurn === undefined || characterInTurn.playerIndex === undefined) {
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
        if (characterInTurn === undefined || characterInTurn.playerIndex === undefined) {
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
    // Add other action cases here based on reference/citadels/src/game_actions.rs
    // Action::GameStart is not a PlayerAction, handle separately if needed or ensure Action type for this function is PlayerAction
    default:
      // Ensure exhaustiveness for PlayerAction if 'action' is narrowed, or handle GameStartAction
      if (action.action === "GameStart") {
        // Logic for GameStart if it were to be handled here
        // For now, assume performAction is for player actions during the game
        throw new Error(`Action type ${action.action} not handled by performAction.`);
      }
      // If action is PlayerAction, this will catch unhandled specific player actions
      throw new Error(`Action type ${(action as any).action} not implemented.`);
  }
}
