if [ "$TRAVIS_BRANCH" == "master" ]; then
	echo "NEVER PUSH ON MASTER!!!"
	exit 1;
fi

export PATH="/usr/bin:$PATH"
git ls-files
cd ./test
echo $PWD
./scripts/build.sh
exit 0
