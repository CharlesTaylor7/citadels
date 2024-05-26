CREATE FUNCTION current_user_id() RETURNS "uuid" 
LANGUAGE PLPGSQL as $$
BEGIN
  RETURN current_setting('citadels.user_id');
END;
$$;

-- profiles --
CREATE TABLE "public"."profiles" (
    "user_id" "uuid" DEFAULT current_user_id() NOT NULL,
    "created_at" timestamp with time zone DEFAULT now() NOT NULL,
    "username" character varying UNIQUE NOT NULL
    PRIMARY KEY ("id"),
    CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "auth"."users"("id") ON DELETE CASCADE
);

ALTER TABLE "public"."profiles" ENABLE ROW LEVEL SECURITY;

-- games --
CREATE TABLE "public"."games" (
    "id" "uuid" DEFAULT gen_random_uuid() NOT NULL,
    "state" "jsonb" NOT NULL,
    "ended_at" timestamp with time zone,
    "started_at" timestamp with time zone DEFAULT "now"() NOT NULL,
    "version" bigint NOT NULL,
    PRIMARY KEY ("id")
);

ALTER TABLE "public"."games" ENABLE ROW LEVEL SECURITY;

-- rooms --
CREATE TABLE "public"."rooms" (
    "id" "uuid" DEFAULT "gen_random_uuid"() NOT NULL,
    "game_id" "uuid",
    "created_at" timestamp with time zone DEFAULT "now"() NOT NULL,
    "game_config" "jsonb" NOT NULL,
    "host_id" "uuid" DEFAULT current_user_id() UNIQUE NOT NULL,
    "player_ids" "text"[] DEFAULT '{current_user_id()::text}'::"text"[] NOT NULL,
    PRIMARY KEY ("id"),
    CONSTRAINT "rooms_game_id_fkey" FOREIGN KEY ("game_id") REFERENCES "public"."games"("id"),
    CONSTRAINT "rooms_host_id_fkey" FOREIGN KEY ("host_id") REFERENCES "public"."profiles"("user_id")
);

ALTER TABLE "public"."rooms" ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Anyone can view room" 
ON "public"."rooms" 
FOR SELECT USING (true);

CREATE POLICY "Owner can create room" 
ON "public"."rooms" 
FOR INSERT WITH CHECK (
  ("host_id" = current_user_id())
);
CREATE POLICY "Owner can close room" 
ON "public"."rooms" 
FOR DELETE USING (
  ("host_id" = current_user_id())
);
CREATE POLICY "Owner can update room ( to kick players)" 
ON "public"."rooms" 
FOR UPDATE USING (
  ("host_id" = current_user_id())
);



/*
-- game_districts --
CREATE TABLE "public"."game_districts" (
    "positions" "jsonb" DEFAULT '{}'::"jsonb" NOT NULL,
    "game_id" bigint NOT NULL,
    "user_id" "uuid" DEFAULT current_user_id() NOT NULL
);

ALTER TABLE ONLY "public"."game_districts"
    ADD CONSTRAINT "game_districts_pkey" PRIMARY KEY ("game_id", "user_id");

ALTER TABLE ONLY "public"."game_districts"
    ADD CONSTRAINT "game_districts_game_id_fkey" FOREIGN KEY ("game_id") REFERENCES "public"."games"("id");

ALTER TABLE ONLY "public"."game_districts"
    ADD CONSTRAINT "game_districts_user_id_fkey1" FOREIGN KEY ("user_id") REFERENCES "public"."profiles"("user_id");


ALTER TABLE "public"."game_districts" ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Anyone can view city district positions" ON "public"."game_districts" FOR SELECT USING (true);
CREATE POLICY "Owner can move districts in their city" ON "public"."game_districts" FOR UPDATE USING (("user_id" = current_user_id()));

-- game_hands --
CREATE TABLE "public"."game_hands" (
    "game_id" bigint NOT NULL,
    "user_id" "uuid" DEFAULT current_user_id() NOT NULL,
    "districts" "text"[] NOT NULL,
    PRIMARY KEY ("game_id", "user_id"),

);


ALTER TABLE ONLY "public"."game_hands"
    ADD CONSTRAINT "game_hands_game_id_fkey" FOREIGN KEY ("game_id") REFERENCES "public"."games"("id");

ALTER TABLE ONLY "public"."game_hands"
    ADD CONSTRAINT "game_hands_user_id_fkey1" FOREIGN KEY ("user_id") REFERENCES "public"."profiles"("user_id");


ALTER TABLE "public"."game_hands" ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Owner can view their hands" ON "public"."game_hands" 
FOR SELECT USING (
  "user_id" = "citadels"."current_user"
);
*/
