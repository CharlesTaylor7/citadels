create extension citext;

create table users (
  id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  username TEXT NOT NULL UNIQUE,
  email CITEXT NOT NULL UNIQUE,
  hashed_password TEXT NOT NULL
);

create table games (
  id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  state JSONB NOT NULL
);

create table rooms (
  id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name TEXT NOT NULL DEFAULT '',
  config JSONB NOT NULL DEFAULT '{}',
  game_id INT REFERENCES games ON DELETE SET NULL
);

create table room_members (
  player_id INT REFERENCES users ON DELETE CASCADE,
  room_id INT REFERENCES rooms ON DELETE CASCADE,
  owner BOOL NOT NULL,
  PRIMARY KEY (player_id, room_id)
);

create unique index unique_room_owner 
  ON room_members (room_id) 
  WHERE owner = true;


create table sessions (
  id int generated always as identity,
  user_id int references users (id) on delete cascade
);
