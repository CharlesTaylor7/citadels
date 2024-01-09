 ## MVP Features
 - all 54 basic districts
 - only 14 of the 30 unique districts
 - select the 14 easiest to implement. Probably end game scoring.
 - implement the 8 base game characters
 - no config, no timers etc
 - card assets? I might just put the card titles and descriptions

## Done !
- Simple Lobby
- 1 active game room, and 1 active game
- WS setup end 2 end with Rust axum, and htmx
- Card data entry for mvp
- Game Setup
- Core action system and game flow 
- UI for District Cards
- UI for Characters
- Allow spectators, instead of bouncing people back to the lobby
- Impersonation

## TODO
- [ ] Lobby
    - [x] html escape names. Askama does this by default.
    - [ ] Enforce unique names in a game
    - [ ] Trim spaces around a name before storing.
    - [ ] Impose username character limit

- [ ] Action logs
    - [x] Deterministic rng
    - [ ] Logs are named after the timestamp
    - [ ] Write to file
    - [ ] Restore from file

- [x] Actions need to self describe what happened for the log.
- [x] Required actions

- Characters 
    - [x] Assassin
    - [x] Thief
    - [x] Magician
    - [x] King
    - [x] Bishop
    - [x] Merchant
    - [x] Architect
    - [x] Warlord
    - [x] Artist
    - Bonus
    - [x] Patrician
    - [x] Navigator
- [x] Scoring
- [x] Unique Districts (implemented 19/30)
- [x] Detect game end

## Improvements
- [ ] Linebreaks, italics and bolds in card descriptions.
- [ ] Take active player at the start of performing an action so it doesn't have to be mutably borrowed a bajillion times.

## UI
- [ ] Action menus
- [ ] Rethink daisyui theme. Maybe something more blue than beige.
- [ ] Action log front and center
- [x] Mark who's hand + city I'm looking at.
- [ ] Point estimate for a player's city
- [ ] Game end overlay with player scores
- [ ] Roles
    - [ ] Unmask player as turn proceeds
    - [ ] Collapsible section of the actions taken
    - [ ] Markers:
        - Killed
        - Robbed

- [ ] Show errors inline
        - https://stackoverflow.com/a/73615279/4875161
- [ ] Player resource summary
    - [x] Highlight active player
    - [x] hand size
    - [x] gold
    - [x] city size
    - [ ] markers: 
        - Crown
        - revealed roles
        - completed city: gold for first,
        - completed city: participation trophy for others


## QA / Playtesting
- [ ] Manually test interactions for every role
- [ ] For every  unique districts.
