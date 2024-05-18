set -eo pipefail

BRANCH=${1:-"main"}

# load .env for supabase env vars
export $(cat .env | xargs)

# generate minified stylesheet
tailwindcss --input tailwind.source.css --output public/styles/index.css --minify

# run supabase migrations
supabase db push

# upload assets to supabase cdn
node upload-assets.js

# deploy to citadels.fly.dev
fly secrets set GIT_SHA=$(git show -s --format=%H)
fly deploy --strategy=immediate -i="ghcr.io/charlestaylor7/citadels:${BRANCH}"
