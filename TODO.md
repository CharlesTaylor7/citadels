 ## Feature set
 - All 54 basic districts
 - 19/30 unique districts
 - 9/30 base game characters + Artist

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


## UI
- [x] Game end overlay with player scores
- [ ] Action menus
    - [x] Assassin
    - [x] Thief
    - [x] Magician
    - [ ] Warlord

- [ ] button labels
- Roles
    - [x] Unmask player as turn proceeds
    - [x] Collapsible section of the actions taken
    - [x] Markers:
        - Killed
        - Robbed

- [ ] Show errors inline
        - https://stackoverflow.com/a/73615279/4875161
- [ ] Show confirmation messages inline
- [ ] Preview how much you would gain from "gain gold from suit x"
- [ ] Player resource summary
    - [x] Highlight active player
    - [x] hand size
    - [x] gold
    - [x] city size
    - [x] score
    - [ ] revealed roles
    - [x] Crown
    - [x] completed city: gold for first,
    - [x] completed city: participation trophy for others

- [x] Crop each district down to a square image, and put its background image onto the square card mockups you built earlier.

## Future Work
- [ ] Linebreaks, italics and bolds in card descriptions.
- [ ] Rethink daisyui theme. Maybe something more blue than beige.
