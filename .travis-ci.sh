export PATH="/usr/bin:$PATH"
sudo apt-get update -qq
sudo apt-get install -qq -y ocaml ocaml-native-compilers menhir m4 llvm opam
opam install llvm.3.6 ocamlfind

echo "OCaml version: "
ocaml -version

cd ./test/scanner
./scripts/build.sh
./scripts/test.sh
./scripts/clean.sh
cd ../parser
./scripts/test.sh
./scripts/clean.sh
echo ""
echo "Testing Hello World!"
echo ""
cd ../../hello_world
./scripts/build.sh > build.log
cat hello_world.cmat | ./cmat.native -c
lli hello_world.ll
echo ""
exit 0
