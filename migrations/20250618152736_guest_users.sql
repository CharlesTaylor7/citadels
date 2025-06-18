CREATE EXTENSION IF NOT EXISTS pgcrypto;

drop table sessions;
alter table users rename to old_users;
alter table room_members rename to old_members;

create table users(
  id uuid primary key default gen_random_uuid(),
  username text unique not null,
  email citext unique,
  hashed_password text
);

insert into users(username, email, hashed_password) select username, email, hashed_password from old_users;

create table room_members (
  room_id INT REFERENCES rooms ON DELETE CASCADE,
  user_id UUID REFERENCES users ON DELETE CASCADE,
  owner BOOL NOT NULL,
  PRIMARY KEY (room_id, user_id)
);

drop index unique_room_owner;
create unique index unique_room_owner 
  ON room_members (room_id) 
  WHERE owner = true;


insert into room_members(room_id, user_id, owner) select room_id, n.id, owner from old_members m join old_users u on u.id = m.player_id join users n on n.username = u.username;


drop table old_members;
drop table old_users;
