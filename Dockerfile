# syntax=docker/dockerfile:1.3.1
FROM rust:1.75-slim-buster as builder
RUN mkdir -p /app
WORKDIR /app

# https://stackoverflow.com/a/58474618
# cache dependencies by building first with an empty main 
RUN echo "fn main() {}" > dummy.rs
COPY .cargo/ .cargo/
COPY vendor/ vendor/
COPY macros/ macros/
COPY macros-impl/ macros-impl/
COPY Cargo.toml Cargo.lock .

RUN sed -i 's#src/main.rs#dummy.rs#' Cargo.toml
RUN cargo build --bin citadels
# RUN cargo build --bin citadels --release 
RUN sed -i 's#dummy.rs#src/main.rs#' Cargo.toml
COPY .env .env
COPY templates/ templates/
COPY src/ src/
RUN cargo build --bin citadels
# RUN cargo build --bin citadels --release

# new layer for smaller image
FROM debian:buster-slim as runner
RUN apt-get update && apt-get install libsqlite3-0 -y
WORKDIR /app
# COPY --from=builder /app/target/release/citadels /app/citadels
COPY --from=builder /app/target/debug/citadels /app/citadels
COPY public/ public/
CMD ["/app/citadels"]

