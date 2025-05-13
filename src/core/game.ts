import { Marker, PlayerId } from "@/core/types";
import { RoleName } from "@/core/roles";
import { DistrictName } from "@/core/districts";
import { PRNG } from "@/server/game/random";
import { Action } from "./actions";
import { playerActionHandlers } from "./action-handlers";

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
  if (action.action === "GameStart") {
    // GameStartAction is not a PlayerAction and should be handled by a different mechanism.
    throw new Error(
      `Action type ${action.action} is not a PlayerAction and cannot be processed by performAction.`,
    );
  } else {
    // In this block, TypeScript infers `action` as `PlayerAction`.
    const handler = playerActionHandlers[action.action];

    if (handler) {
      // The `action` object (now inferred as PlayerAction) is passed to the specific handler.
      // The `playerActionHandlers` map ensures that `handler` expects the correct subtype of `PlayerAction`.
      return handler(game, action);
    } else {
      // Fallback for PlayerAction types that are not yet in the map or are misspelled.
      throw new Error(
        `No handler implemented for action type ${action.action}.`,
      );
    }
  }
}
