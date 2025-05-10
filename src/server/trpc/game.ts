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
  heartbeat: loggedInProcedure
    .input(z.object({ roomId: z.string() }))
    .subscription(async function* ({ input: { roomId } }) {
      for await (const event of on(eventEmitter, roomId)) {
        yield event;
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
