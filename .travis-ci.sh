export PATH="/usr/bin:$PATH"
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers menhir

echo "OCaml version: "
ocaml -version

cd ./test/scanner
./scripts/build.sh
./scripts/test.sh
./scripts/clean.sh
cd ../parser
./scripts/test.sh
./scripts/clean.sh
exit 0
