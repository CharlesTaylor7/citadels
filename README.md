## Citadels
This is an online implementation of the multiplayer Citadels game

## Dev commands

For most commands: use mprocs.

For deployments:
```
fly deploy --local-only
```

### Installing deps


To install a new dep:
1. Comment out the 2nd line from .cargo/config.toml. So it looks like:
`# replace-with = "vendored-sources"`
2. `cargo add foo`
3. `cargo vendor`
4. Uncomment .cargo/config.toml. 

You might think this is a lot of friction for adding deps. This is a good thing. Vendoring deps makes it so that the docker image can build more readily, and makes our codebase more resistant to supply chain attacks. 
The friction makes me think twice before adding a new dep.

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
