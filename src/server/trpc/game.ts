import { z } from "zod";
import { router, loggedInProcedure } from ".";
import { EventEmitter, on } from "node:events";

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
