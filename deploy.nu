#!/usr/bin/env nu

def main [branch: string = "main"] {
 let api_key = $env.CURRENT_FILE 
  | path dirname 
  | path join .env 
  | open --raw $in 
  | from toml 
  | get UPLOADTHING_API_KEY

  # TODO: upload public dir

  # generate minified stylesheet
  tailwindcss --input tailwind.source.css --output public/styles/index.css --minify

  # deploy to citadels.fly.dev
  fly secrets set GIT_SHA=$(git show -s --format=%H)
  fly deploy --strategy=immediate -i $"ghcr.io/charlestaylor7/citadels:($branch)"
}
