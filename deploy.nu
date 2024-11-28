def main [branch: string = "main"] {
  let api_key = open .env | get UPLOAD_THING_API_KEY

  # generate minified stylesheet
  tailwindcss --input tailwind.source.css --output public/styles/index.css --minify

  # upload assets to supabase cdn
  node upload-assets.js

  # deploy to citadels.fly.dev
  fly secrets set GIT_SHA=$(git show -s --format=%H)
  fly deploy --strategy=immediate -i $"ghcr.io/charlestaylor7/citadels:($branch)"
}
