## Citadels
This is a web app for playing the card game [Citadels](https://boardgamegeek.com/boardgame/478/citadels).

## Goals
- Support all characters and roles
- Learn htmx, practice web dev


## Feature set
 - All 30 unique districts
 - All 27 game characters.
 - Game config for picking roles and districts.


## Tech Stack 
Fly.io for deployments
neondb for managed postgres
Frontend:
- React
- Angular
- React query
- Spa routing
- tailwind + daisyui for styling & component



Backend
- poem
- poem-openapi

## Releases
How to release:
- `./scripts/deploy-local.nu`
Or use GH image created on merge to main with:
- `./scripts/deploy.nu`.

## Secret management
In prod use `fly secrets set`.
In dev, use the .env file.

Non secret env vars can go in fly.toml

## Development
Get mprocs, npm, and rust toolchain installed

1. Copy sample.env to .env
2. npm i
3. mprocs
