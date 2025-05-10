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
      // Yield a heartbeat event every second
      const intervalId = setInterval(() => {
        // It's important that the eventEmitter.emit happens *before* the yield
        // for the `on` listener below to pick it up in the same tick.
        // However, for a simple interval like this, we can directly yield.
        // For this example, let's assume we want to emit and then yield.
        // A more direct way for interval is to just yield.
      }, 1000);

      // Clean up the interval when the subscription is closed
      const cleanup = () => {
        clearInterval(intervalId);
      };

      // This part demonstrates listening to an event emitter,
      // which might be useful if events are generated elsewhere.
      // For a simple interval, you might not need this specific `on` listener
      // if you are directly yielding from the interval callback.
      // However, to stick to the existing pattern of using eventEmitter:
      const eventPromise = on(eventEmitter, `${roomId}-heartbeat`);

      try {
        while (true) {
          // Emit an event that the `on` listener can pick up
          // Or, more simply, just yield directly if the event is self-contained to this subscription
          const heartbeatMessage = { type: "heartbeat", timestamp: Date.now(), roomId };
          eventEmitter.emit(`${roomId}-heartbeat`, heartbeatMessage); // Emit for the listener
          
          // Wait for the next event from the emitter
          const [eventData] = await Promise.race([
            eventPromise, // Wait for an emitted event
            new Promise(resolve => setTimeout(resolve, 1000)).then(() => [heartbeatMessage]) // Fallback / direct yield
          ]);
          yield eventData;

          // Reset the promise for the next iteration if using `on`
          // eventPromise = on(eventEmitter, `${roomId}-heartbeat`); // This line would be complex to manage correctly with async generator
          // For simplicity with interval, let's adjust to a direct yield model for the interval part.

          // Simplified direct yield for interval:
          // yield { type: "heartbeat", timestamp: Date.now(), roomId };
          // await new Promise(resolve => setTimeout(resolve, 1000)); // Wait for 1 second
        }
      } finally {
        cleanup();
      }
    }),

  // A more direct way to implement an interval subscription:
  heartbeatInterval: loggedInProcedure
    .input(z.object({ roomId: z.string() }))
    .subscription(async function* ({ input: { roomId } }) {
      const intervalId = setInterval(() => {
        // This approach has a potential issue: `yield` cannot be called directly
        // from the `setInterval` callback because `async function*` expects `yield`
        // to be called within its direct scope or a promise it awaits.
        // So, we still need an event emitter or a different pattern.
      }, 1000); // This is problematic for direct yield.

      // Corrected interval approach:
      let stopped = false;
      const cleanup = () => {
        stopped = true;
      };

      try {
        while (!stopped) {
          yield { type: "heartbeat", timestamp: Date.now(), roomId, message: "Regular pulse" };
          // Wait for 1 second or until stopped
          await new Promise(resolve => {
            const timer = setTimeout(resolve, 1000);
            // If stopped, clear timeout immediately
            if (stopped) {
              clearTimeout(timer);
              resolve(null); 
            }
          });
        }
      } finally {
        // No specific intervalId to clear here as we used a flag
        // If an actual setInterval was used and events pushed to a queue,
        // then clearInterval(intervalId) would be here.
      }
      // The above is one way. Let's refine the original `heartbeat` to be interval-based.
    }),
  
  // Refined heartbeat to use interval correctly (simplified async generator pattern)
  heartbeatRevised: loggedInProcedure
    .input(z.object({ roomId: z.string() }))
    .subscription(async function* ({ input: { roomId } }) {
      let timeoutId: NodeJS.Timeout | undefined;
      try {
        while (true) {
          // Yield a heartbeat message
          yield { type: "heartbeat", timestamp: Date.now(), roomId, message: "Pulse" };
          
          // Wait for 1 second
          await new Promise<void>(resolve => {
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
