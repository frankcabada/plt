#Merge to master if build succeeds
git fetch
git checkout -b master --track origin/master || exit
git merge "$TRAVIS_BRANCH" || exit #may need to change back to $TRAVIS_COMMIT
