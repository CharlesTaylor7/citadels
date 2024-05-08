set -eo pipefail

# load .env for supabase env vars
export $(cat .env | xargs)

# generate minified stylesheet
tailwindcss --input tailwind.source.css --output styles/index.min.css --minify

# run supabase migrations
supabase db push

# upload stylesheet to supabase cdn
node upload-css.js
# yes y | supabase storage --experimental rm ss:///styles/index.css || true
# supabase storage --experimental cp styles/index.min.css ss:///styles/index.css

# deploy to citadels.fly.dev
fly secrets set GIT_SHA=$(git show -s --format=%H)
fly deploy --strategy=immediate -i=ghcr.io/charlestaylor7/citadels:main
