## UI
The lobby page shows active games and not started games in separate lists. Either two columns, or two tabs.

## User actions
### anon users
- Can spectate a room

### Logged in
- can change their username.
- can join a room.
- can create a new room, if they're not currently in one.
- can leave a room

### The room host
- can edit the game config
- can start the game
- can close the room
- can kick people from the room

## Profiles
Users can set their username. It is autopopulated from their Discord username or their Discord "Global name".


## Rooms
can be public or private.
    - public rooms are visible on the lobby page
    - private rooms are accessible by url only. 
have 1 host with extra permissions.
A user can only be in one room at a time.
Every room has a unique url.
A room can be in one of 2 states:
 - waiting to start a game.
 - an active game.

Rooms that are inactive are automatically deleted.

## Games
A room can have at most one active game.
You can visit a game by either using its room url or its game url. the room url redirects to the active game url if there is one.

Ideally Games will not be deleted. I'd like to keep these around for statistics purposes. If I exceed the storage quota for Supabase, I may resort to clearing out games that are over a month old.


A room is consider inactive if there is 
    - no active game AND
    - the last game was completed over an hour ago or the room has never hosted a game.
