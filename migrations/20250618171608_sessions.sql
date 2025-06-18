create table sessions(
  id uuid primary key default gen_random_uuid(),
  user_id uuid not null references users(id) on delete cascade,
  created_at timestamptz not null default current_timestamp
);

alter table games add column created_at timestamptz not null default current_timestamp;
alter table rooms add column created_at timestamptz not null default current_timestamp;
alter table users add column created_at timestamptz not null default current_timestamp;



