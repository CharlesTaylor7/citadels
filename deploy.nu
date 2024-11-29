#!/usr/bin/env nu

def main [branch: string = "main"] {
  fly deploy --strategy=immediate -i $"ghcr.io/charlestaylor7/citadels:($branch)"
}
