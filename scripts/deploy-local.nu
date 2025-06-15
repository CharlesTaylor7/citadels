#!/usr/bin/env nu

def main [] {
  docker build . -t citadels --platform linux/amd64
  fly deploy --strategy=immediate --local-only -i citadels:latest
}
