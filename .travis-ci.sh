if [ "$TRAVIS_BRANCH" == "master" ]; then
	echo "NEVER PUSH ON MASTER!!!"
	exit 1; 
fi

git checkout master || exit
git merge "$TRAVIS_COMMIT" || exit
git push