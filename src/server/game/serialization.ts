import { GameState } from "@/core/game";
import { prngReplacer, prngReviver } from "@/server/game/random";

export function serializeGame(game: GameState): string {
  return JSON.stringify(game, prngReplacer);
}

export function deserializeGame(json: string): GameState {
  return JSON.parse(json, prngReviver);
}
