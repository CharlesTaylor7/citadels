 ## Feature set
 - 19/30 unique districts
 - 11/27 game characters.
 - Dragon


## Shortlist
- [ ] OOB swaps and updates targeted only at the players and screen elements that need to change. The current logic just liberally pushes a full page to everyone which wipes client state, like the opened logs, and the position(s) of dragged items.
- [ ] Regression Tests / e2e tests. the Logical bugs, like the inability to take the crown or collect gold from non-blue districts could have been caught easily by tests.
    - 
- [ ] Backup game state to db, by storing rng seed, and the action log.
- [ ] regression testing through saving action logs to sqlite.
- [ ] Where do destroyed districts go? they ought to get discarded, but I don't think I implemented it properly.
- [ ] Show all abilities but greyed out as the turn progresses.


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
    - [ ] serialized to sqlite db
    - [ ] Restore from sqlite

- UX
    - [x] Better labels, e.g. "gain n gold from suit x"
    - [ ] Show confirmation messages inline
    - [ ] Show errors inline
        - https://stackoverflow.com/a/73615279/4875161
- [ ] Quickcheck tests

## Future Work

- [ ] Linebreaks, italics and bolds in card descriptions.
- [ ] No user feedback when a submission fails
- [ ] Can embed lobby view into game view
- [ ] no confirmation messages
- [ ] inconsistent log format and tenses
- [ ] Observatory is not noted, but library is.


## Playtest feedback
- [ ] Notification bell for start of turn.
    - [x] audio tag, with ws event listener
    - [ ] Server needs to send an html element with data-ring-bell, but only once when the turn begins
    - [ ] Server shuffles audio files to send.
- [ ] role and district description should have dedicated info icon for bringing up their tooltips.
- [ ] Warlord menu should make it obvious the cost to destroy from the great wallled city is higher.

### Done
- [x] Collect gold for suit, always counts blue instead of the correct suit.
- [x] Bishop should only protect your city when revealed, not if killed.
- [x] Warlord can destroy own city district.
- [x] click instead of hover to view someone's city. Persist the state, don't restore your city on hover back.
- [x] Take crown action, can't actually be taken. This should be a required action for the king and patrician.
- [x] Keep role logs open, instead of just the active role.
	- [x] Open them all at the start of a round.
	- When sending logs, send them as oob-swaps 

## Cleo's easter eggs
- [x] Dragging the dragon out of his section play's Mr. Brightside. Putting him back pauses it.
