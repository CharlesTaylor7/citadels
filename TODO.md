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

- [ ] Role Call phase
- [ ] Characters (only these 9)
    - [ ] Assassin
    - [ ] Thief
    - [ ] Magician
    - [ ] King
    - [ ] Bishop
    - [ ] Merchant
    - [ ] Architect
    - [ ] Warlord
    - [ ] Artist
- [ ] Unique Districts (only 14 of the easiest)

## UI
- [x] Flex row of flex columns instead of grid layout
- [ ] Rethink daisyui theme. Maybe something more blue than beige.
- [ ] Action log front and center
- [ ] Mark who's hand + city I'm looking at.
- [ ] Point estimate for a player's city
- [ ] Roles
    - [x] Tooltip to the right for each role
    - [ ] Unmask player as turn proceeds
    - [ ] Collapsible section of the actions taken
    - [ ] Markers:
        - Killed
        - Stolen

- [ ] Show errors inline
        - https://stackoverflow.com/a/73615279/4875161
- [ ] Player resource summary
    - Highlight active player
    - hand size
    - gold
    - city size
    - markers: 
        - Crown
    - revealed roles
