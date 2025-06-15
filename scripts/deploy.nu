#!/usr/bin/env nu
use "./bash-env.nu"

def main [branch: string = "main"] {
  bash-env .prod.env
  sqlx migrate run
  fly deploy --strategy=immediate -i $"ghcr.io/charlestaylor7/citadels:($branch)"
}
