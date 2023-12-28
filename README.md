## Citadels
This is an online implementation of the multiplayer Citadels game

## Dev commands

Use steeloverseer

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
    - websockets
    - stm
    - concurrent-hashtable
    - unagi-chan (if needed)
        - so far sticking to writing to stm & iorefs instead of message passing.
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

Seriously considering switching to Rust for better performance.
Considering using jinja templates or something with template fragments. See: https://htmx.org/essays/template-fragments/
Also a nice benefit to build my pages in a way that is less dependent on the server language.

I wonder if these are compatible enough with jinja to use ?
Haskell:
- https://github.com/tdammers/ginger
- No Fragment support. :(


Rust:
- https://github.com/mitsuhiko/minijinja 
- https://docs.rs/template-fragments/latest/template_fragments/

My progress so far is that I built a prototype lobby. I built a websocket server in Haskell. I'm concerned about development velocity and production performance.

- Slow dev velocity because the combination of steel overseer + GHC + warp/wai is slow to reload my changes
- Slow dev velocity because haskell prevents use of fine grained natural mutablity.
    - Immutable persistent data is really a bad fit for this app. Its not like saves me from side effects, it just makes the side effects a lot more expensive in space and time complecity.
- No template fragments.
- I'm a prototype the lobby in rust now and see how that does.


It's important to not be demoralized and stay positive on a project. 


### Twain 

I like wai, and Warp, but I dislike Twain. It's simpler than Spock and Scotty, but it has a lot of pitfals that break my intuition on how request routing should work.

I want a framework that produces an error and stack trace when request parsing fails. It shouldn't just keep trying routes and then 404.

I shouldn't be able to forget to `send`. AGain this 404s instead of being a type error. 

### Scotty

Makes it hard to embed Websocket handling. I ended up switching to Twain because Twain atleast made it easy to embed a websocket response handler

### STM

stm is probably overkill. It's probably not great for performance either. I may be able to refactor to using a single IORef, and this utility:
https://hackage.haskell.org/package/base-4.16.3.0/docs/Data-IORef.html#v:atomicModifyIORef-39-

## Initial Features

What to do for authentication
Static list of players

Need to support 2 to 3 player games only.

I need the ability to kick anyone

I will not require passwords.
I will deal with spammers / bots on an as need basis.
I will make it so you need a game code to join a room, and to see a running game as a spectator.
