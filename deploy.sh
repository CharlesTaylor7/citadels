NODE_PATH=/opt/homebrew/lib/node_modules tailwindcss --input tailwind.source.css --output styles/index.css --minify
supabase storage rm ss:///styles/index.css
supabase storage cp styles/index.css ss:///styles/index.css
fly secrets set GIT_SHA=$(git show -s --format=%H)
fly deploy --strategy=immediate -i=ghcr.io/charlestaylor7/citadels:main

