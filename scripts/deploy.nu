#!/usr/bin/env nu
use "./bash-env.nu"

def main [branch: string = "main"] {
  let prod_env = open .env.prod
  | split column "=" -n 2
  | transpose -r
  | into record

  with-env $prod_env { sqlx migrate run }
  fly deploy --strategy=immediate -i $"ghcr.io/charlestaylor7/citadels:($branch)"
}
