set -eo pipefail

# deploy to citadels.fly.dev
fly secrets set GIT_SHA=$(git show -s --format=%H)
fly deploy --strategy=immediate -i=ghcr.io/charlestaylor7/citadels:main
