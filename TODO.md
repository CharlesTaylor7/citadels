## Shortlist
- [x] Magistrate
    - Reveal warrant menu

- [ ] Rework warlord menu as a modal

- [ ] Blackmailer
    - Bribe menu

- [ ] Spy
    - Spy menu

- [ ] Abbot 
    - abbot menu

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


## Playtest 1 feedback
- [ ] Notification bell for start of turn.
    - [x] audio tag, with ws event listener
    - [ ] Server needs to send an html element with data-ring-bell, but only once when the turn begins
    - [ ] Server shuffles audio files to send.
- [ ] role and district description should have dedicated info icon for bringing up their tooltips.
- [ ] Warlord menu should make it obvious the cost to destroy from the great wallled city is higher.

## Playtest 2
- [x] Role config menu is clipped on smaller screens
- [ ] flex-wrap my-roles. Basically, they should stack vertically if I narrow my window
- [ ] "Players' turn" could have more context. show role + name. Put word draft if they are drafting.
- [ ] dismissible popup when your district is detroyed, just shows inline a picture of the destroyed district.
- [ ] museum auto tooltip hides the artifacts
- [ ] Clicking End turn crashes the game
- [ ] Allow names with some punctuation. Will need to url encode names in city request
- [ ] Limit width and apply text-wrap to right sidebar usernames. Long usernames shouldn't distort the whole page.
- [ ] William requested Wizard as next role. Full random, more card draw in the game.

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
