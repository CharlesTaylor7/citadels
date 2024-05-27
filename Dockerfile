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
COPY vendor/ vendor/

# Build deps
RUN RUST_BACKRACE=1 cargo chef cook --release --recipe-path recipe.json

# Build application
COPY . .
# Temporary: remove impersonate flag later
RUN RUST_BACKTRACE=1 cargo build --release --bin citadels --features=impersonate

FROM alpine AS runtime
WORKDIR /app
COPY --from=builder /app/target/release/citadels /usr/local/bin
ENTRYPOINT ["/usr/local/bin/citadels"]
