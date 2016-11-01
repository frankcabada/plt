if [ "$TRAVIS_BRANCH" == "master" ]; then
	echo "NEVER PUSH ON MASTER!!!"
	exit 1;
fi

export PATH="/usr/bin:$PATH"
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers

echo "OCaml version: "
ocaml -version

cd ./test
./scanner/scripts/build.sh
./scanner/scripts/test.sh
exit 0
