# Intro
This is my plan to resuccistate the project.

## Poem
Build a dummy project using poem and SQLx to write a simple REST API server.
Try to suss out pain points of building it out to be the citadels api server.
Figure it if SSE is a good fit.

Aspects to evaluate
- [x] SSE
- [x] SQLx
- [x] Misc headers, Location, etc. 
- [x] Customizing status codes
- [x] error handling, thiserror & color-eyre integration
- [x] Cookies
- [ ] Auth
  - poem-auth
  - poem-grants

- [ ] ArcStr, can it easily work?

## Auth
- managed auth: auth0
- manual auth
- reuse node server auth

## Wasm

Figure out if we can compile the citadels library to wasm, and then call from a node server.

## Glue
Glue together the best parts of previous implementations.
Angular custom elements
React based lobby
Rust based core game logic

## Deploy
Redpeloy to Azure. Limited to 2 free servers on Fly.io, but I want to learn Azure. So move it over and see how it goes.

Current plan is to use Terraform and deploy this as-is. Because right now the project is not dependent on a database. 

https://citadels-server.gentlestone-0f9d12de.eastus.azurecontainerapps.io
portal.azure.com


So far Azure has been insanely complicated and obnoxious. I should try aws at some point, but thinking about trying easier options like Railway now.



## Test

Build test coverage by generating random games, and checking the game never gets into an illegal state or panics.

## Refactor 
Cleanup all existing sources of panics and do smarter more careful error handling

do a cleaner validate and rollback by either:

immutable updates only to game state

Cloning the game before applying game actions partially.

Use ast-grep to locate problematic constructs


