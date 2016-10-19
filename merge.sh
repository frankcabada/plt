#Merge to master if build succeeds
git fetch || exit
git checkout master || exit
git merge "$TRAVIS_BRANCH" || exit #may need to change back to $TRAVIS_COMMIT
git push

