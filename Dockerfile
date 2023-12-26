FROM haskell:9.4.8-slim-buster as builder
RUN cabal update
RUN mkdir -p /app
WORKDIR /app
COPY . .
RUN cabal build citadels-lib --dependencies-only
RUN cabal build citadels-server
RUN cabal install citadels-server --installdir /app

FROM debian:buster-slim as runner
COPY --from=builder /app/dist-newstyle/sdist/ /app/dist-newstyle/sdist/
COPY --from=builder /app/public/ /app/public/
COPY --from=builder /app/citadels-server /app/citadels-server

WORKDIR /app
EXPOSE 8080

CMD ["./citadels-server"]
