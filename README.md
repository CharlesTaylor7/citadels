## Citadels
This is an online implementation of the multiplayer Citadels game

## Dev commands

See mprocs file

## Implementation notes

- The district card data will be in a csv file format, so it can be compiled from a spreadsheet.

## Feature Ideas
- Base game
- Custom Cards 
- Expansion Characters
- Optional Timers
    - Basic
    - Chess Clock (Pokemon showdown)

## Tech Stack 
- Haskell 
    - lucid2
    - twain
    - unagi-chan
    - concurrent-hashtable
    - websockets
- HTMX
- Hyperscript (if needed)


Docs:
- https://htmx.org/docs/
- https://hyperscript.org/docs/
- https://hackage.haskell.org/package/twain-2.1.2.0/docs/Web-Twain.html
- https://hackage.haskell.org/package/lucid2-0.0.20230706/docs/Lucid-Html5.html
- https://hackage.haskell.org/package/concurrent-hashtable-0.1.8/docs/Data-HashTable.html
- https://hackage.haskell.org/package/websockets-0.12.7.3/docs/Network-WebSockets.html#t:Connection
- https://hackage.haskell.org/package/unagi-chan-0.4.1.4/docs/Control-Concurrent-Chan-Unagi.html


## Initial Features
What to do for authentication
Static list of players

Need to support 2 to 3 player games only.

I need the ability to kick anyone

I will not require passwords.
I will deal with spammers / bots on an as need basis.
I will make it so you need a game code to join a room, and to see a running game as a spectator.


 ## TODO
- [x] Scaffold haskell project
- [x] Dockerfile
- [x] Deploy to fly.io
- [ ] Lobby
    - [x] Render static list of players
    - [ ] Render dynamic list of players
    - [ ] Render name register form
    - [ ] Hookup state

 - [x] POC for html rendering
 - [x] POC for api htmx swapping
 - [ ] POC for Web sockets comms

- [ ] Card Assets

## Roadblocks
- [x] Haskell Wai server not showing stacktraces
- [x] send ws data
