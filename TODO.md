 Just build the thing. 
 I can performance tune later.
 I can harden security later.
 I can oxidize the project later.
 I can switch to jinja templates later.
 I can abandon the project altogether if its too distracting.

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



## TODO
- [x] Game Setup
    - [x] shuffle seating order
    - [x] shuffle districts
    - [x] collect character cards
    - [x] Deal initial hands and gold
- [ ] Role Draft phase
- [ ] Role Call phase
- [ ] Allow spectators, instead of bouncing people back to the lobby
- [x] Show each player their hand
- [ ] District Card template
    - [ ] change cost background to match suit
- [ ] Character Card template
    - [ ] change rank background to something that doesn't conflict with a suit. Maybe orange?
    - [ ] card border if its affiliated with a suit.

- [ ] Debug Utilities
    - [x] Impersonation
    - [ ] indicate impersonated character
    - [ ] refactor impersonate route
- [ ] Turn Log
- [ ] Round Log
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
