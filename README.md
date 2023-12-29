## Citadels
This is an online implementation of the multiplayer Citadels game

## Dev commands

For most commands: use mprocs.

For deployments:
```
cargo vendor # only if dependencies have changed
fly deploy
```

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
Seriously considering switching to Rust for better performance.
Considering using jinja templates or something with template fragments. See: https://htmx.org/essays/template-fragments/
Also a nice benefit to build my pages in a way that is less dependent on the server language.

I wonder if these are compatible enough with jinja to use ?
Haskell:
- https://github.com/tdammers/ginger
- No Fragment support. :(

Tech Stack:
- htmx
- hyperscript (if needed)
- Rust
    - axum
    - minijinja
        - https://github.com/mitsuhiko/minijinja 
        - https://docs.rs/template-fragments/latest/template_fragments/
    
Build the lobby with this tech stack and evaluate


## Initial Features

What to do for authentication
Static list of players

Need to support 2 to 3 player games only.

I need the ability to kick anyone

I will not require passwords.
I will deal with spammers / bots on an as need basis.
I will make it so you need a game code to join a room, and to see a running game as a spectator.
