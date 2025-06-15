-- Add migration script here
-- Add migration script here
alter table logs 
drop column player_index;


alter table logs
add column player_name text;
