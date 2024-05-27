set -eo pipefail

BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
echo "Deploying branch: $BRANCH"

# generate minified stylesheet
tailwindcss --input tailwind.source.css --output public/styles/index.css --minify

# run supabase migrations
# temporary: Don't do this after your first stable release
yes | supabase db reset --linked
# supabase db push

# upload assets to supabase cdn
node --env-file=deploy/.env deploy/upload-assets.js

# deploy to citadels.fly.dev
fly secrets set GIT_SHA=$(git show -s --format=%H)
fly deploy --strategy=immediate -i="ghcr.io/charlestaylor7/citadels:${BRANCH}"
