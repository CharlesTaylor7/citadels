set -e

# load .env for supabase env vars
export $(cat .env | xargs)

# generate minified stylesheet
NODE_PATH=/opt/homebrew/lib/node_modules tailwindcss --input tailwind.source.css --output styles/index.css --minify

# run supabase migrations
# supabase db push

# upload stylesheet to supabase cdn
supabase storage --experimental rm ss:///styles/index.css
supabase storage --experimental cp styles/index.css ss:///styles/index.css

# deploy to citadels.fly.dev
fly secrets set GIT_SHA=$(git show -s --format=%H)
fly deploy --strategy=immediate -i=ghcr.io/charlestaylor7/citadels:main
