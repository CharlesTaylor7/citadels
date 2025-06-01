-- Add migration script here

alter table users alter column
username SET NOT NULL;

alter table users add constraint
unique_username UNIQUE (username);


