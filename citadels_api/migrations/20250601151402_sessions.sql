-- Add migration script here
create table sessions (
  id int generated always as identity,
  user_id int references users (id) on delete cascade
);

alter table users
add column guest bool not null default true;

-- comment on users.guest is 'a guest user account is one without a password; users can join games with just a username without registering a password';
