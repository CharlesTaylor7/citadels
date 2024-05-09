create policy "Anyone can view city district positions"
on "public"."game_districts"
as permissive
for select
to public
using (true);


create policy "Owner can move districts in their city"
on "public"."game_districts"
as permissive
for update
to public
using ((user_id = auth.uid()));


create policy "Owner can view their hands"
on "public"."game_hands"
as permissive
for select
to public
using ((user_id = auth.uid()));


create policy "Anyone can view"
on "public"."rooms"
as restrictive
for select
to public
using (true);


create policy "Owner can close room"
on "public"."rooms"
as permissive
for delete
to public
using ((owner_id = auth.uid()));


create policy "Owner can create room"
on "public"."rooms"
as permissive
for insert
to public
with check ((owner_id = auth.uid()));


create policy "Owner can update room ( to kick players)"
on "public"."rooms"
as restrictive
for update
to public
using ((owner_id = auth.uid()));



