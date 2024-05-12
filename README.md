## Citadels
This is a web app for playing the card game [Citadels](https://boardgamegeek.com/boardgame/478/citadels).

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

## Tech Stack 
Tech Stack:
- Frontend:
    - htmx w/ extensions
        - json-enc (serde with form data doesn't work very well)
        - ws
        - clientsidetemplates
        - idiomorph
    - hyperscript
    - interactjs for drag 'n drop

- Backend 
    - Rust, stable compiler. No nightly features
    - axum
    - Askama for templating. (Jinja clone for Rust)

Supabase is handling:
- Auth layer
    - email, password
    - Discord SSO
- Postgres database
    - games
    - rooms
    - realtime websocket access for both of these.
    - sqlite for two use cases:
        - game backups via action logs
        - saving preferred game configuration

## Logging
The Rust logger offers five levels:
    - error
    - warn
    - info
    - debug
    - trace

This is excessive, and I will try to stick to 3:
    - error
    - debug
    - info

## Env & Secret management
Script to generate a new signing key:
```bash
node -e "console.log(require('crypto').randomBytes(64).toString('hex'))"
```
In prod use `fly secret`.
In dev, use the .env file.

Env vars for development go in:
- .env

For production go in:
- Use the `fly.toml`: for non secret configured env vars.
- Use `fly secret`, for the secret env vars.

Don't use `.config/cargo.toml` or the env! macro. We need runtime env lookup, not compile time.
