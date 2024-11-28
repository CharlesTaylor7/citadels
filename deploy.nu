def main [branch: string = "main"] {

  # generate minified stylesheet
  tailwindcss --input tailwind.source.css --output public/styles/index.css --minify
  let api_key = open .env | get UPLOADTHING_API_KEY

  # TODO: upload public dir

  # deploy to citadels.fly.dev
  fly secrets set GIT_SHA=$(git show -s --format=%H)
  fly deploy --strategy=immediate -i $"ghcr.io/charlestaylor7/citadels:($branch)"
}
