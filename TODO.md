## Goals
- Support all characters and roles
- Make it fun
- Easter eggs
- Bots?
- Stable, well tested.
- Learn htmx, practice web dev
- Learn how to setup OAuth?


## Feature set
 - 25/30 unique districts
 - 11/27 game characters.
 - Dragon


## Shortlist
- [ ] OOB swaps and updates targeted only at the players and screen elements that need to change. The current logic just liberally pushes a full page to everyone which wipes client state, like the opened logs, and the position(s) of dragged items.
- [ ] Restore game state from lobby

## Ideas
- [ ] Nerfed Asssassin idea: Kill the role and all its abilities, but the player can still take normal actions. Gather, build, any district abilities
- [ ] Make it possible to save a set of roles to use, for config.
- [ ] Detect when building is impossible.

- Game Configuration
    - [ ] Pick roles
    - [ ] Pick unique districts

- Lobby
    - [x] html escape names. Askama does this by default.
    - [ ] Make some characters illegal: commas, whitespace
    - [ ] Enforce unique names in a game
    - [ ] Trim spaces around a name before storing.
    - [ ] Impose username character limit

- Action logs
    - [x] Deterministic rng
    - [x] serialized to sqlite db
    - [ ] Restore from sqlite

- UX
    - [x] Better labels, e.g. "gain n gold from suit x"
    - [ ] Show confirmation messages inline
    - [ ] Show errors inline
        - https://stackoverflow.com/a/73615279/4875161
    - [ ] Provide user feedback when a submission fails
    - [ ] Make log format and tenses consistent


## Playtest feedback
- [ ] Notification bell for start of turn.
    - [x] audio tag, with ws event listener
    - [ ] Server needs to send an html element with data-ring-bell, but only once when the turn begins
    - [ ] Server shuffles audio files to send.
- [ ] role and district description should have dedicated info icon for bringing up their tooltips.
- [ ] Warlord menu should make it obvious the cost to destroy from the great wallled city is higher.


## Cleo's easter eggs
- [x] Dragging the dragon out of his section play's Mr. Brightside. Putting him back pauses it.
    - [ ] Need to send ws updates as oob swaps so as to not disrupt the video player dom state.
