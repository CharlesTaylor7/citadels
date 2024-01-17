## Goals
- Support all characters and roles
- Make it fun
- Easter eggs
- Stable, well tested.
- Learn htmx, practice web dev
- apply 12 factor app principles:
    https://www.12factor.net/


## Feature set
 - 26/30 unique districts
 - 16/27 game characters.
 - Dragon


## TODO
- [ ] Edit game config from lobby
- [ ] Rework menus to use drag and drop.
- [ ] Rework menus as modals.
- [ ] OOB swaps and updates targeted only at the players and screen elements that need to change. The current logic just liberally pushes a full page to everyone which wipes client state, like the opened logs, and the position(s) of dragged items.
- [ ] Restore game state from lobby
- [ ] Detect when building is impossible.

- Game Configuration
    - [ ] Pick roles
    - [ ] Pick unique districts

## Ideas
- [ ] Make it possible to save a set of roles to use, for config.


- Lobby
    - [x] html escape names. Askama does this by default.
    - [ ] Make some characters illegal: commas, whitespace
    - [ ] Enforce unique names in a game
    - [ ] Trim spaces around a name before storing.
    - [ ] Impose username character limit

- Action logs
    - [ ] Restore game from sqlite

- UX
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

## Feature Ideas
- Custom Card editor
- Custom Role editor
- Optional Timers
    - Basic
    - Chess Clock (Pokemon showdown)
- kick bots / spammers
- auth?
- multi room
- game config
- spectator support

## Custom Roles
- [ ] Nerfed Asssassin: Kill the role and all its abilities, but the player can still take normal actions. Gather, build, any district abilities

## Cleo's easter eggs
- [x] Dragging the dragon out of his section play's Mr. Brightside. Putting him back pauses it.
    - [ ] Need to send ws updates as oob swaps so as to not disrupt the video player dom state.
