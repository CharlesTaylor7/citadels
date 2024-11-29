# syntax=docker/dockerfile:1.3.1
# https://github.com/LukeMathWalker/cargo-chef?tab=readme-ov-file#without-the-pre-built-image
FROM lukemathwalker/cargo-chef:latest-rust-1.80.1 AS chef
WORKDIR /app

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder 
COPY --from=planner /app/recipe.json recipe.json

# build dependencies
RUN cargo chef cook --release --recipe-path recipe.json

# build application
COPY . .
RUN cargo build --release --bin citadels_server

FROM debian:bookworm-slim AS runtime
WORKDIR /app
COPY --from=builder /app/target/release/citadels_server /usr/local/bin
COPY public/ public/
ENTRYPOINT ["/usr/local/bin/citadels_server"]
