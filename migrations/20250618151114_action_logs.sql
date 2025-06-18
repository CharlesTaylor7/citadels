create table action_logs (
  id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  created_at timestamptz not null default current_timestamp,
  action jsonb not null,
  player_name text,
  game_id int not null references games(id) on delete cascade
);

insert into action_logs (created_at, action, player_name, game_id) select created_at, action, player_name, game_id from logs;

drop table logs;
