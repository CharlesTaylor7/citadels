create function public.new_profile_after_user_signup()
returns trigger
language plpgsql
security definer set search_path = ''
as $$
begin
  INSERT INTO public."profiles" (id, username)
  VALUES (NEW.id, COALESCE(NEW.raw_user_meta_data#>'{custom_claims,global_name}', NEW.raw_user_meta_data->'full_name'));
  RETURN NEW;
EXCEPTION WHEN OTHERS THEN
  GET STACKED DIAGNOSTICS 
    message = MESSAGE_TEXT,
    exception = PG_EXCEPTION_DETAIL;
  
  INSERT INTO public."profiles" (id, username)
  VALUES (NEW.id, message)
  RETURN NEW;
end;
$$;

create trigger on_auth_user_created
  after insert or update on auth.users
  for each row execute function public.new_profile_after_user_signup();
