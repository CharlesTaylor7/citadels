fly secrets set GIT_SHA=$(git show -s --format=%H)
fly deploy
