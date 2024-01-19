## Citadels
This is a web app for playing the card game [Citadels](https://boardgamegeek.com/boardgame/478/citadels).

## Goals
- Support all characters and roles
- Make it fun
- Easter eggs
- Stable, well tested.
- Learn htmx, practice web dev
- apply 12 factor app principles:
    https://www.12factor.net/

## Feature set
 - 26/30 unique districts
 - 16/27 game characters.
 - Dragon
 - Game config for picking roles and districts.


## Dev commands
For most commands: use mprocs.

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

## Releases
How to release:
- Bump the version in fly.toml
- Run: `fly deploy`
- Update changelog
- git commit changes
- Git tag the release 

## Secret management

Script to generate a new signing key:
```js
require('crypto').randomBytes(64).toString('hex')
```
In prod use `fly secret`.
In dev, use the .env file.

Non secret env vars can go in fly.toml
