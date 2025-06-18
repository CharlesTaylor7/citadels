# syntax=docker/dockerfile:1.3.1
# https://github.com/LukeMathWalker/cargo-chef?tab=readme-ov-file#without-the-pre-built-image
FROM rust:1.87-slim-bookworm AS builder
WORKDIR /app

COPY . .
RUN cargo build --release --bin citadels_api

FROM debian:bookworm-slim AS runtime
WORKDIR /app
COPY --from=builder /app/target/release/citadels_api /usr/local/bin
COPY public/ public/
ENTRYPOINT ["/usr/local/bin/citadels_api"]
