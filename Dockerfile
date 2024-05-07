# syntax=docker/dockerfile:1.3.1
FROM lukemathwalker/cargo-chef:latest-rust-1 AS chef
WORKDIR /app

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder 
COPY --from=planner /app/recipe.json recipe.json

# Copy vendored deps
COPY .cargo/ .cargo/
COPY vendor/ vendor/
COPY macros/ macros/
COPY macros-impl/ macros-impl/

# Build dependencies - this is the caching Docker layer!
RUN cargo chef cook --release --recipe-path recipe.json
# Build application
COPY . .
RUN cargo build --release --bin app

# We do not need the Rust toolchain to run the binary!
FROM debian:bookworm-slim AS runtime
WORKDIR /app
COPY --from=builder /app/target/release/citadels /usr/local/bin
COPY public/ public/
ENTRYPOINT ["/usr/local/bin/citadels"]

# FROM rust:1.75-slim-buster as builder
# RUN mkdir -p /app
# WORKDIR /app
# 
# RUN echo "fn main() {}" > dummy.rs
# COPY Cargo.toml Cargo.lock .
# 
# RUN sed -i 's#src/main.rs#dummy.rs#' Cargo.toml
# RUN cargo build --bin citadels
# # RUN cargo build --bin citadels --release 
# RUN sed -i 's#dummy.rs#src/main.rs#' Cargo.toml
# COPY .env .env
# COPY templates/ templates/
# COPY src/ src/
# RUN cargo build --bin citadels
# # RUN cargo build --bin citadels --release
# 
# # new layer for smaller image
# FROM debian:buster-slim as runner
# WORKDIR /app
# # COPY --from=builder /app/target/release/citadels /app/citadels
# COPY --from=builder /app/target/debug/citadels /app/citadels
# COPY public/ public/
# CMD ["/app/citadels"]
# 
