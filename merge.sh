#Merge to master if build succeeds
git checkout master || exit
git merge "$TRAVIS_COMMIT" || exit
git push

