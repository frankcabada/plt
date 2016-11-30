export PATH="/usr/bin:$PATH"

sudo apt-get install m4 llvm software-properties-common
sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -qq -y ocaml ocaml-native-compilers menhir opam
opam init -a
eval `opam config env`
opam install -y depext
opam depext llvm.3.4
opam install -y llvm.3.4 ocamlfind

cd ./test/scanner
./scripts/build.sh
./scripts/test.sh
./scripts/clean.sh
cd ../parser
./scripts/test.sh
./scripts/clean.sh
cd ../../hello_world
./scripts/build.sh > build.log
cat hello_world.cmat | ./cmat.native -c
lli hello_world.ll > hello_world.res
diff -q hello_world.out hello_world.res
./scripts/clean.sh

exit 0
