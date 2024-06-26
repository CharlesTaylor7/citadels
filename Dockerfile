# syntax=docker/dockerfile:1.3.1
# https://github.com/LukeMathWalker/cargo-chef?tab=readme-ov-file#without-the-pre-built-image
FROM rust:alpine AS chef
RUN apk add --no-cache musl-dev
RUN cargo --version
RUN cargo install cargo-chef

WORKDIR /app

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder 
COPY --from=planner /app/recipe.json recipe.json

COPY macros/ macros/
COPY macros-impl/ macros-impl/

RUN cargo chef cook --release --recipe-path recipe.json
# Build application
COPY . .
RUN cargo build --release --bin citadels

FROM alpine AS runtime
WORKDIR /app
COPY --from=builder /app/target/release/citadels /usr/local/bin
COPY public/ public/
ENTRYPOINT ["/usr/local/bin/citadels"]
