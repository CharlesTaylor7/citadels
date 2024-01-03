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
- [ ] Use Result instead of Options
- [ ] Role Draft phase
- [ ] Role Call phase
- [ ] Allow spectators, instead of bouncing people back to the lobby
- [x] Show each player their hand
- [x] District Card template
    - [x] change cost background to match suit
- [x] Character Card template
    - [x] change rank background to something that doesn't conflict with a suit. Maybe orange?
    - [x] card rank background if its affiliated with a suit.

- [ ] Debug Utilities
    - [x] Impersonation
    - [x] refactor impersonate route
    - [x] indicate impersonated character
    - [x] Dev feature is independent of debug_assertions

- [ ] Left Sidebar Roles & Round Log 
    - [x] Tooltip to the right for each
    - [ ] Collapsible section of the actions taken

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
