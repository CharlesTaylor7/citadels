import {
  sqliteTable as table,
  text,
  integer,
  uniqueIndex,
} from "drizzle-orm/sqlite-core";
import { sql, type InferSelectModel } from "drizzle-orm";

export const users = table("users", {
  id: integer("id").primaryKey(),
  username: text("username").notNull().unique(),
  hashedPassword: text("hashed_password").notNull(),
});

export const sessions = table("sessions", {
  id: text("id").primaryKey(),
  userId: integer("user_id")
    .notNull()
    .references(() => users.id, { onDelete: "cascade" }),
  expiresAt: integer("expires_at", {
    mode: "timestamp",
  }).notNull(),
});

export const room_members = table(
  "room_members",
  {
    // this ensures each player is in only 1 room at a time
    playerId: integer("player_id")
      .primaryKey()
      .references(() => users.id, { onDelete: "cascade" }),
    roomId: text("room_id")
      .notNull()
      .references(() => rooms.id, { onDelete: "cascade" }),
    owner: integer("owner", { mode: "boolean" }).notNull().default(false),
  },
  (table) => [
    // this ensures each room has at most 1 owner
    uniqueIndex("room_owners")
      .on(table.roomId)
      .where(sql`owner = true`),
  ],
);

export const rooms = table("rooms", {
  id: text("id").primaryKey(),
  name: text("name").notNull().default(""),
  // json object of options
  options: text("options").notNull(),

  // multiple games can happen in the same room, just not at the same time
  gameId: integer("game_id").references(() => games.id, {
    onDelete: "cascade",
  }),
});

export const games = table("games", {
  id: integer("id").primaryKey(),
  // json of the game state
  state: text("state").notNull(),
  // json array of actions
  // the current state should ideally be derivable by replaying all the actions
  actions: text("actions").notNull(),
});

export const tables = { users, sessions, games, rooms, room_members };

export type User = InferSelectModel<typeof users>;
export type Session = InferSelectModel<typeof sessions>;
export type Room = InferSelectModel<typeof rooms>;
export type Game = InferSelectModel<typeof games>;
