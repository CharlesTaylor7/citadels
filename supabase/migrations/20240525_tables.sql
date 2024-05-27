-- Function that allows us to use RLS policies for either direct database connections 
-- or Supabase data api calls. (postgrest or graphql).
CREATE FUNCTION current_user_id() RETURNS "uuid" 
LANGUAGE PLPGSQL 
AS $$
DECLARE 
  user_id "uuid";
BEGIN
  SELECT current_setting('citadels.user_id', true) INTO user_id;
  RETURN (
    CASE WHEN user_id IS NOT NULL THEN
      user_id
    ELSE
      auth.uid()
    END
  );
END;
$$;

CREATE FUNCTION current_room_id() RETURNS "uuid" 
LANGUAGE PLPGSQL 
SECURITY DEFINER
AS $$
BEGIN
  RETURN (
    SELECT room_id FROM room_members WHERE user_id = current_user_id()
  );
END;
$$;



-- profiles --
CREATE TABLE profiles (
    "user_id" "uuid" DEFAULT current_user_id() NOT NULL,
    "created_at" timestamp with time zone DEFAULT now() NOT NULL,
    "username" character varying UNIQUE NOT NULL,
    PRIMARY KEY ("user_id"),
    CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "auth"."users"("id") ON DELETE CASCADE
);

ALTER TABLE profiles ENABLE ROW LEVEL SECURITY;


CREATE POLICY "Anyone can view any profile" 
ON profiles
FOR SELECT USING ( 
  true
);

CREATE POLICY "Anyone can create their own profile" 
ON profiles
FOR INSERT WITH CHECK ( 
  user_id = current_user_id() 
);


CREATE POLICY "Anyone can update their own profile" 
ON profiles
FOR UPDATE USING ( 
  user_id = current_user_id() 
);


-- games --
CREATE TABLE games (
    "id" "uuid" DEFAULT gen_random_uuid() NOT NULL,
    "state" "jsonb" NOT NULL,
    "ended_at" timestamp with time zone,
    "started_at" timestamp with time zone DEFAULT "now"() NOT NULL,
    "version" bigint NOT NULL,
    PRIMARY KEY ("id")
);

ALTER TABLE games ENABLE ROW LEVEL SECURITY;

-- rooms --
CREATE TABLE rooms (
    "id" "uuid" PRIMARY KEY DEFAULT "gen_random_uuid"(),
    "game_id" "uuid" REFERENCES games("id"),
    "created_at" timestamp with time zone DEFAULT "now"() NOT NULL,
    "private" bool DEFAULT false,
    "game_config" "jsonb" NOT NULL,
    -- Unique means a user can only host 1 room at a time.
    -- Foreign key means the user has to have setup their profile.
    "host_id" "uuid" DEFAULT current_user_id() UNIQUE NOT NULL REFERENCES profiles("user_id")
    -- "player_ids" "uuid"[] DEFAULT ARRAY[current_user_id()] NOT NULL,
    -- no more than 9 players
    -- CHECK (cardinality(player_ids) < 10),
    -- host is one of the players
    -- CHECK (host_id = any(player_ids))
);

ALTER TABLE "rooms" ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Anyone can view a public room" 
ON rooms 
FOR SELECT USING (
  NOT rooms.private
);

CREATE POLICY "Anyone can view a room they are in" 
ON rooms 
FOR SELECT USING (
  rooms.id = current_room_id()
);

CREATE POLICY "If you create a room, you have to host" 
ON rooms
FOR INSERT WITH CHECK (
  rooms.host_id = current_user_id()
);

CREATE POLICY "Host can close room" 
ON rooms
FOR DELETE USING (
  rooms.host_id = current_user_id()
);

CREATE POLICY "Host can update room" 
ON rooms
FOR UPDATE USING (
  rooms.host_id = current_user_id()
);


-- room_members --
CREATE TABLE room_members (
    "user_id" "uuid" DEFAULT current_user_id() REFERENCES profiles("user_id"),
    "room_id" "uuid" REFERENCES rooms("id"),
    PRIMARY KEY ("user_id")
);

CREATE POLICY "If you can view the room, you can view members" 
ON room_members 
FOR SELECT USING (
  EXISTS ( 
    SELECT 1 as _ FROM rooms WHERE rooms.id = room_members.room_id
  )
);


CREATE POLICY "A user can join a room" 
ON room_members
FOR INSERT WITH CHECK (
  room_members.user_id = current_user_id()
);

CREATE POLICY "A user can leave a room"
ON room_members
FOR DELETE USING (
  (room_members.user_id = current_user_id())
);

CREATE POLICY "A host can kick a user"
ON room_members
FOR DELETE USING (
  EXISTS ( 
    SELECT 1 as _ FROM rooms WHERE room_members.room_id = rooms.id AND rooms.host_id = current_user_id() 
  )
);

/*
-- game_districts --
CREATE TABLE "game_districts" (
    "positions" "jsonb" DEFAULT '{}'::"jsonb" NOT NULL,
    "game_id" bigint NOT NULL,
    "user_id" "uuid" DEFAULT current_user_id() NOT NULL
);

ALTER TABLE ONLY "game_districts"
    ADD CONSTRAINT "game_districts_pkey" PRIMARY KEY ("game_id", "user_id");

ALTER TABLE ONLY "game_districts"
    ADD CONSTRAINT "game_districts_game_id_fkey" FOREIGN KEY ("game_id") REFERENCES "games"("id");

ALTER TABLE ONLY "game_districts"
    ADD CONSTRAINT "game_districts_user_id_fkey1" FOREIGN KEY ("user_id") REFERENCES "profiles"("user_id");


ALTER TABLE "game_districts" ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Anyone can view city district positions" ON "game_districts" FOR SELECT USING (true);
CREATE POLICY "Owner can move districts in their city" ON "game_districts" FOR UPDATE USING (("user_id" = current_user_id()));

-- game_hands --
CREATE TABLE "game_hands" (
    "game_id" bigint NOT NULL,
    "user_id" "uuid" DEFAULT current_user_id() NOT NULL,
    "districts" "text"[] NOT NULL,
    PRIMARY KEY ("game_id", "user_id"),

);


ALTER TABLE ONLY "game_hands"
    ADD CONSTRAINT "game_hands_game_id_fkey" FOREIGN KEY ("game_id") REFERENCES "games"("id");

ALTER TABLE ONLY "game_hands"
    ADD CONSTRAINT "game_hands_user_id_fkey1" FOREIGN KEY ("user_id") REFERENCES "profiles"("user_id");


ALTER TABLE "game_hands" ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Owner can view their hands" ON "game_hands" 
FOR SELECT USING (
  "user_id" = "citadels"."current_user"
);
*/
