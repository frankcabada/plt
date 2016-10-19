git checkout master || exit
git merge "$TRAVIS_COMMIT" || exit
git push

