-- Step 1: Ensure 1 room & 1 game id exists
INSERT INTO games(state)  SELECT '{}';
Insert into rooms(game_id) VALUES (null);

-- Step 2: Alter logs table to add game_id column
ALTER TABLE logs ADD COLUMN game_id INT NOT NULL DEFAULT 1;

-- Step 3: Alter logs table to add foreign key constraint
ALTER TABLE logs ADD CONSTRAINT logs_game_fkey FOREIGN KEY (game_id) REFERENCES games(id) ON DELETE CASCADE;

-- Step 4: Drop default so new logs must be explicitly assigned to a game
ALTER TABLE logs alter column game_id drop default;

