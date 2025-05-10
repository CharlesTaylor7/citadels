# angular
- [x] compile to custom element
- [x] Embed custom element
- [ ] Angular reload dev mode
- [~] Type def for custom element
- [ ] Get tailwind working across both
- [ ] split server and core directories based on dependencies
- [ ] yarn workspaces?

# Shortlist   
- [ ] Login/signup
  - [ ] Can view errors on page
- [ ] Card viewer
- [ ] lobby
    - [x] flicker on load. use loader?
    - [x] can transfer owner
    - [x] Can claim ownership
    - [x] can create room
    - [x] Can display rooms
    - [x] query updates for room
    - [x] live updates for rooms
    - [x] Can join room
    - [x] can leave room
    - [x] Can start game
- [ ] We should not be sending stacktraces back to the client
- [ ] Github action to build docker image
- [ ] Deploy from docker image

# Backend
- [x] setup seedable rng
- [] Setup route to apply action, and serialize action into game model.
Need to serialize actions and make game replayable from actions
- [] tests for game service

# Frontend
## Game
- [ ] Each player can see their dealt district cards
- [ ] Draft
- [ ] Need to start porting templates

## Lobby
- [ ] Websocket for rooms
- [] can transfer ownership of a room to someone else
- [x] Can create room
- [x] Should only be allowed to be in one room at a time
- [x] If in a room, and its game has started redirect to that game
- [x] Can Join a room
- [x] can leave a room
- [x] Owner can start game


# PLAN
The ai doesn't like to port files over about 500 LOC
Use AI assistants to port templates and game logic.
Wire up everything yourself. AI is good at porting across languages and giving me a starting point, but is bad at gluing the pieces together IMHO.


