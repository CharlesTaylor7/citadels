import { EventEmitter } from "stream";

class NotificationService {
  private eventEmitter = new EventEmitter();

  public notifyRoom(notify: RoomNotification) {
    this.eventEmitter.emit(notify.roomId, notify.message);
  }

  public notifyPlayer(notify: PlayerNotification) {
    this.eventEmitter.emit(`${notify.roomId}-${notify.userId}`, notify.message);
  }
}

interface RoomNotification {
  roomId: string;
  message: string;
}

interface PlayerNotification extends RoomNotification {
  userId: number;
}
