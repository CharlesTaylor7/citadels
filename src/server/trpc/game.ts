import { z } from "zod";
import { router, loggedInProcedure } from ".";
import { EventEmitter, on } from "node:events";
import { PlayerActionSchema } from "@/core/actions";
import { games, room_members, rooms } from "../schema";
import { eq } from "drizzle-orm";
import { deserializeGame, serializeGame } from "@/server/game/serialization";

const eventEmitter = new EventEmitter();

interface RoomNotification {
  roomId: string;
  message: string;
}

interface PlayerNotification {
  roomId: string;
  userId: number;
  message: string;
}

function notifyRoom(notify: RoomNotification) {
  eventEmitter.emit(notify.roomId, notify.message);
}

function notifyPlayer(notify: PlayerNotification) {
  eventEmitter.emit(`${notify.roomId}-${notify.userId}`, notify.message);
}

export const gameRouter = router({
  // todo: aider
  act: loggedInProcedure
    .input(PlayerActionSchema)
    .mutation(async ({ input, ctx: { userId, db } }) => {
      const row = await db
        .select({ game: games })
        .from(room_members)
        .innerJoin(rooms, eq(rooms.id, room_members.roomId))
        .innerJoin(games, eq(games.id, rooms.gameId))
        .where(eq(room_members.playerId, userId))
        .get();

      if (!row) {
        throw new Error("not part of a game");
      }
      // row.game.actions.push(input);

      console.log(input);
    }),
  // message every second
  heartbeat: loggedInProcedure.subscription(async function* () {
    let timeoutId: NodeJS.Timeout | undefined;
    try {
      while (true) {
        // Yield a heartbeat message
        yield {
          type: "heartbeat",
          timestamp: Date.now(),
          message: "Pulse",
        };

        // Wait for 1 second
        await new Promise<void>((resolve) => {
          timeoutId = setTimeout(resolve, 1000);
        });
        timeoutId = undefined; // Clear after it resolves, before next iteration's yield
      }
    } finally {
      // If the generator is exited (e.g., client disconnects, error),
      // clear any pending timeout.
      if (timeoutId) {
        clearTimeout(timeoutId);
      }
    }
  }),

  generalNotifications: loggedInProcedure
    .input(z.object({ roomId: z.string() }))
    .subscription(async function* ({ input: { roomId } }) {
      for await (const event of on(eventEmitter, roomId)) {
        yield event;
      }
    }),

  myNotifications: loggedInProcedure
    .input(z.object({ roomId: z.string() }))
    .subscription(async function* ({ ctx: { userId }, input: { roomId } }) {
      for await (const event of on(eventEmitter, `${roomId}-${userId}`)) {
        yield event;
      }
    }),
});
