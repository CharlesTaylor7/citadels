create extension if not exists citext;
create extension if not exists pgcrypto;

create table games (
  id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  created_at timestamptz not null default current_timestamp,
  state JSONB NOT NULL,
  active boolean not null default true
);

create table rooms (
  id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  created_at timestamptz not null default current_timestamp,
  name TEXT NOT NULL DEFAULT '',
  config JSONB NOT NULL,
  game_id INT REFERENCES games(id) ON DELETE SET NULL
);

create table action_logs (
  id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  created_at timestamptz not null default current_timestamp,
  action jsonb not null,
  player_index int,
  game_id int not null references games(id) on delete cascade
);

create table users(
  id int generated always as identity primary key,
  created_at timestamptz not null default current_timestamp,
  username text unique not null,
  email citext unique,
  hashed_password text
);


create table room_members (
  room_id INT NOT NULL REFERENCES rooms(id) ON DELETE CASCADE,
  user_id INT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  owner BOOL NOT NULL,
  PRIMARY KEY (room_id, user_id)
);

create unique index unique_room_owner 
  ON room_members (room_id) 
  WHERE owner = true;


create table sessions(
  id uuid primary key default gen_random_uuid(),
  created_at timestamptz not null default current_timestamp,
  user_id int not null references users(id) on delete cascade
);
