if [ "$TRAVIS_BRANCH" == "master" ]; then
	echo "NEVER PUSH ON MASTER!!!"
	exit 1;
fi

git ls-files
cd ./test
./scripts/build.sh
./scripts/test.sh
exit 0
