#!/usr/bin/env nu

def main [branch: string = "main"] {
  tailwindcss --input tailwind.source.css --output public/styles/index.css --minify

  # deploy to citadels.fly.dev
  fly secrets set GIT_SHA=$(git show -s --format=%H)
  fly deploy --strategy=immediate -i $"ghcr.io/charlestaylor7/citadels:($branch)"
}
