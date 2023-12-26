## Citadels
This is an online implementation of the multiplayer Citadels game

## Dev commands

See mprocs file

## Implementation notes

- The district card data will be in a csv file format, so it can be compiled from a spreadsheet.


## Tech Stack / implementation notes

I've considered a few options. Rust, Purescript, Typescrpt. Halogen, React, NExt.js, etc.


Going to try:
- Haskell (blaze + Scotty)
- HTMX
- Hyperscript (if needed)

Docs:
- https://jaspervdj.be/blaze/tutorial.html
- https://github.com/scotty-web/scotty
- https://htmx.org/docs/
- https://hyperscript.org/docs/


What to do for authentication
Static list of players

Need to support 2 to 3 player games only.

I need the ability to kick anyone


I will not require passwords.
I will deal with spammers / bots on an as need basis.
I will make it so you need a game code to join a room, and to see a running game as a spectator.


Websockets:
https://gist.github.com/andrevdm/9560b5e31933391694811bf22e25c312#file-scotty_websockets-hs


Build out lobby with this tech stack and then decide whether to proceed or rewrite with a different stack.

 
 ## TODO
- [x] Scaffold haskell project
- [ ] Dockerfile
- [ ] Deploy to fly.io
- [ ] Lobby
    - [ ] Render a list of rooms
    - [ ] Render a list of players
    - [ ] Update players / rooms with websockets

 - [ ] POC for html rendering
 - [ ] POC for api htmx swapping
 - [ ] POC for Web sockets comms

 - [ ] Assets
- [ ] Features
