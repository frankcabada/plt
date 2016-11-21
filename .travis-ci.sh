export PATH="/usr/bin:$PATH"

sudo apt-get install m4 llvm software-properties-common
sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -qq -y ocaml ocaml-native-compilers menhir opam
opam init -a
eval `opam config env`
opam depext llvm.3.4
opam install -y llvm.3.4 ocamlfind

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
echo "-----------------------------------------"
echo "|           Testing Hello World!        |"
echo "-----------------------------------------"
echo ""
cd ../../hello_world
./scripts/build.sh > build.log
cat hello_world.cmat | ./cmat.native -c
lli hello_world.ll
echo ""
echo ""
exit 0
