import { z } from "zod";
import { router, loggedInProcedure } from ".";
import { PlayerActionSchema } from "@/core/actions";
import { games, room_members, rooms } from "../schema";
import { eq } from "drizzle-orm";
import { deserializeGame, serializeGame } from "@/server/game/serialization";
import { performAction } from "@/server/game/game";
import { roomEvents, playerEvents } from "@/server/game/notifications";

// Types for the result object with discriminated union
type Success<T> = {
  data: T;
  error: null;
};

type Failure<E> = {
  data: null;
  error: E;
};

type Result<T, E = unknown> = Success<T> | Failure<E>;

// Main wrapper function
export const gameRouter = router({
  // todo: aider
  act: loggedInProcedure
    .input(PlayerActionSchema)
    .mutation(async ({ input, ctx: { userId, db } }) => {
      const row = await db
        .select({ game: games, roomId: rooms.id })
        .from(room_members)
        .innerJoin(rooms, eq(rooms.id, room_members.roomId))
        .innerJoin(games, eq(games.id, rooms.gameId))
        .where(eq(room_members.playerId, userId))
        .get();

      if (!row) {
        throw new Error("not part of a game");
      }
      const game = deserializeGame(row.game.state);
      performAction(game, input, row.roomId);
      await db
        .update(games)
        .set({ state: serializeGame(game) })
        .where(eq(games.id, row.game.id));
    }),

  generalNotifications: loggedInProcedure
    .input(z.object({ roomId: z.string() }))
    .subscription(async function* ({ input: { roomId } }) {
      for await (const event of roomEvents(roomId)) {
        yield event;
      }
    }),

  myNotifications: loggedInProcedure
    .input(z.object({ roomId: z.string() }))
    .subscription(async function* ({ ctx: { userId }, input: { roomId } }) {
      for await (const event of playerEvents(roomId, userId)) {
        yield event;
      }
    }),
});
