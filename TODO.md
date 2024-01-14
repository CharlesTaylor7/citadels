 ## Feature set
 - All 54 basic districts
 - 19/30 unique districts
 - 9/30 base game characters + Artist

## Ideas
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
    - [ ] Write to file
    - [ ] Restore from file
- UX
    - [x] Better labels, e.g. "gain n gold from suit x"
    - [ ] Show confirmation messages inline
    - [ ] Show errors inline
        - https://stackoverflow.com/a/73615279/4875161

## Future Work
- [ ] Linebreaks, italics and bolds in card descriptions.
- [ ] Rethink daisyui theme. Maybe something more blue than beige.
- [ ] More Characters:
    - [x] Patrician
    - [_] Navigator
    - [_] Scholar
    - [_] Abbot
- [ ] More Districts 


## Severe Issues
- [ ] No user feedback when a submission fails


## Minor Issues
- [ ] Can embed lobby view into game view
- [ ] no confirmation messages
- [ ] inconsistent log format and tenses
- [ ] Observatory is not noted, but library is.

## Playtest feedback
- [ ] Keep role logs open, instead of just the active role.
	- [x] Open them all at the start of a round.
	- When sending logs, send them as oob-swaps so they don't close the open ones.
- [x] Take crown action, can't actually be taken. This should be a required action for the king and patrician.
- [ ] Notification bell for start of turn.
    - [x] audio tag, with ws event listener
    - [ ] Server needs to send an html element with data-ring-bell, but only once when the turn begins
    - [ ] Server shuffles audio files to send.

- [ ] role and district description should have dedicated info icon for bringing up their tooltips.
- [ ] click instead of hover to view someone's city. Persist the state, don't restore your city on hover back.
	- this makes it work for ipad better, and makes it so players can inspect a city more closely.
- [x] Collect gold for suit, always counts blue instead of the correct suit.
- [ ] Make it possible to save a set of roles to use, for config.
- [x] Bishop should only protect your city when revealed, not if killed.
- [ ] Nerfed Asssassin idea: Kill the role and all its abilities, but the player can still take normal actions. Gather, build, any district abilities
- [ ] Warlord menu should make it obvious the cost to destroy from the great wallled city is higher.
- [ ] Detect when building is impossible.
