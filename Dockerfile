# https://stackoverflow.com/a/58474618
# cache dependencies by building first with an empty main 

FROM rust:1.75-slim-buster as builder
RUN mkdir -p /app
WORKDIR /app

RUN echo "fn main() {}" > dummy.rs
COPY .cargo/ .cargo/
COPY vendor/ vendor/
COPY Cargo.toml Cargo.lock .

RUN sed -i 's#src/main.rs#dummy.rs#' Cargo.toml
RUN cargo build --release
RUN sed -i 's#dummy.rs#src/main.rs#' Cargo.toml
COPY .env .env
COPY templates/ templates/
COPY src/ src/
RUN cargo build --release

# new layer for smaller image
FROM debian:buster-slim as runner
WORKDIR /app
COPY --from=builder /app/target/release/citadels /app/citadels
CMD ["/app/citadels"]

