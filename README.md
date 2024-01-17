## Citadels
This is an online implementation of the multiplayer Citadels game

## Dev commands

For most commands: use mprocs.

How to release:
1. Bump the version in the Cargo.toml
2. Bump the version in fly.toml
3. Update changelog.
4. Git tag the release. 
5. Run: `fly deploy`

## Logging

The logger offers five levels:
    - error
    - warn
    - info
    - debug
    - trace

This is excessive, and I will try to stick to 3:
    - error
    - debug
    - info


### Installing deps


To install a new dep:
1. Comment out the 2nd line from .cargo/config.toml. So it looks like:
`# replace-with = "vendored-sources"`
2. `cargo add foo`
3. `cargo vendor`
4. Uncomment .cargo/config.toml. 

You might think this is a lot of friction for adding deps. This is a good thing. Vendoring deps makes it so that the docker image can build more readily, and makes our codebase more resistant to supply chain attacks. 
The friction makes me think twice before adding a new dep.



## Tech Stack 
Tech Stack:
- Frontend:
    - htmx
    - hyperscript (as needed)
    - inline js
    - interactjs for drag 'n drop
- Backend 
    - Rust, stable compiler. No nightly features
    - axum
    - Askama for templating. (Jinja clone for Rust)

Evaluating:
    - sqlite for two use cases:
        - game backups via action logs
        - saving preferred game configuration
    


