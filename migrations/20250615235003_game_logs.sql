-- Step 1: Ensure game with id = 1 exists
INSERT INTO games(state)  SELECT '{}' WHERE NOT EXISTS (SELECT 1 FROM games WHERE id = 1);

-- Step 2: Alter logs table to add game_id column
ALTER TABLE logs ADD COLUMN game_id INT DEFAULT 1;

-- Step 3: Alter logs table to add foreign key constraint
ALTER TABLE logs ADD CONSTRAINT logs_game_fkey FOREIGN KEY (game_id) REFERENCES games(id) ON DELETE CASCADE;

