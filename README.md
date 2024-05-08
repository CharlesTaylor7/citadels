## Citadels
This is a web app for playing the card game [Citadels](https://boardgamegeek.com/boardgame/478/citadels).

## Goals
- Support all characters and roles
- Learn htmx, practice web dev


## Feature set
 - All 30 unique districts
 - All 27 game characters.
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

## Tech Stack 
Tech Stack:
- Frontend:
    - htmx
    - hyperscript (as needed)
    - interactjs for drag 'n drop
- Backend 
    - Rust, stable compiler. No nightly features
    - axum
    - Askama for templating. (Jinja clone for Rust)

Evaluating:
    - sqlite for two use cases:
        - game backups via action logs
        - saving preferred game configuration

## Releases & monitoring
Github handles publishing a docker image on every push to `main`.  
Deployments are done manually with the `deploy.sh` script.
This is because the services are not yet stateless, and I don't want to disrupt ongoing games with a deploy.

Monitor the app on these dashboards: 
- https://fly.io/apps/citadels/
- https://supabase.com/dashboard/project/ryvsflpspddwwacxrnst

Fly.io hosts and runs the Rust axum server as a docker image.
Supabase is used for:
- auth
- cdn for static assets
- database for game data

To release:
- push changes to the repo's `main` branch. this will trigger a docker build.
- Run the deploy.sh script when its done. You can do this automatically with:
```
gh run watch && ./deploy.sh
```

## Secret management

Script to generate a new signing key:
```bash
node -e "console.log(require('crypto').randomBytes(64).toString('hex'))"
```
In prod use `fly secret`.
In dev, use the .env file.

Non secret env vars can go in fly.toml
