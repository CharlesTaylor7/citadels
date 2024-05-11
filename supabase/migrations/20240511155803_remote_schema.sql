drop policy "Owner can close room" on "public"."rooms";

drop policy "Owner can create room" on "public"."rooms";

drop policy "Owner can update room ( to kick players)" on "public"."rooms";

alter table "public"."rooms" drop constraint "rooms_owner_id_fkey";

alter table "public"."rooms" drop constraint "rooms_owner_id_key";

drop index if exists "public"."rooms_owner_id_key";

alter table "public"."games" drop column "created_at";

alter table "public"."games" add column "ended_at" timestamp with time zone;

alter table "public"."games" add column "started_at" timestamp with time zone not null default now();

alter table "public"."games" add column "version" bigint not null;

alter table "public"."rooms" drop column "owner_id";

alter table "public"."rooms" add column "game_config" jsonb not null;

alter table "public"."rooms" add column "host_id" uuid not null default auth.uid();

alter table "public"."rooms" add column "player_ids" text[] not null default '{auth.uid()::text}'::text[];

CREATE UNIQUE INDEX rooms_owner_id_key ON public.rooms USING btree (host_id);

alter table "public"."rooms" add constraint "rooms_host_id_fkey" FOREIGN KEY (host_id) REFERENCES auth.users(id) not valid;

alter table "public"."rooms" validate constraint "rooms_host_id_fkey";

alter table "public"."rooms" add constraint "rooms_owner_id_key" UNIQUE using index "rooms_owner_id_key";

create policy "Owner can close room"
on "public"."rooms"
as permissive
for delete
to public
using ((host_id = auth.uid()));


create policy "Owner can create room"
on "public"."rooms"
as permissive
for insert
to public
with check ((host_id = auth.uid()));


create policy "Owner can update room ( to kick players)"
on "public"."rooms"
as restrictive
for update
to public
using ((host_id = auth.uid()));



