import { on } from "events";
import { EventEmitter } from "stream";

const eventEmitter = new EventEmitter();
export function roomEvents(roomId: string): AsyncIterable<string[]> {
  return on(eventEmitter, roomId);
}

export function playerEvents(
  roomId: string,
  userId: number,
): AsyncIterable<string[]> {
  return on(eventEmitter, `${roomId}-${userId}`);
}

export function notifyRoom(notify: RoomNotification) {
  eventEmitter.emit(notify.roomId, notify.message);
}

export function notifyPlayer(notify: PlayerNotification) {
  eventEmitter.emit(`${notify.roomId}-${notify.userId}`, notify.message);
}

export interface RoomNotification {
  roomId: string;
  message: string;
}

export interface PlayerNotification extends RoomNotification {
  userId: number;
}
