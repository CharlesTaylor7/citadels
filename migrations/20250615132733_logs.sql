-- Add migration script here
create table logs (
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  action JSONB,
  player_index int
)
