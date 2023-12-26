FROM haskell:9.4.8-slim-buster as builder
RUN mkdir -p /app
WORKDIR /app
COPY . .
RUN cabal build -O2
RUN cabal install --installdir /app

FROM debian:buster-slim as runner
COPY --from=builder /app/dist-newstyle/sdist/ /app/dist-newstyle/sdist/
COPY --from=builder /app/public/ /app/public/
COPY --from=builder /app/citadels /app/server

WORKDIR /app
EXPOSE 8080

CMD ["./server"]
