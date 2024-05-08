-- storage setup --
insert into "storage"."buckets"("id", "name", "public", "allowed_mime_types")
values ('styles', 'styles', true, '{"text/css"}')
on conflict ("id")
do update set
  "name" = excluded.name, 
  "public" = excluded.public,
  "allowed_mime_types" = excluded.allowed_mime_types;
