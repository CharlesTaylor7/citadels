## Shortlist
- [ ] Abbot 
    - collect resources menu
    - tax richest menu
    - fixup magician, change format to not require "Select<>"

- [ ] Custom extractor for form url encoded data:
    https://github.com/tokio-rs/axum/blob/main/examples/customize-extractor-error/src/custom_extractor.rs

- [ ] Rework warlord menu as a modal
- [ ] Blackmailer
    - Bribe menu

- [ ] Spy
    - Spy menu


- [ ] Diplomat 
- [ ] Marshal 

## Ideas
- [ ] Keyboard based warrant assignment
- [ ] Multi room support
- [ ] Make log format and tenses consistent
- [ ] Rework menus to use drag and drop.
- [ ] Rework menus as modals.
- [ ] Detect when building is impossible.
- [ ] Make it possible to save a set of roles to use, for config.
- [ ] Restore game from sqlite


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
