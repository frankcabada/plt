#/usr/bin/sh
read -p "Press [Enter] key to start Hello World Demo"
cd hello_world

echo "Compiling source"
./scripts/build.sh > build.log
echo "Compilation complete"

./cmat.native -c hello_world.cmat hello_world.ll
echo ""
read -p "Press [Enter] key to run Hello World"
echo ""
lli hello_world.ll
echo ""

echo "Cleaning up"
./scripts/clean.sh
echo "Presentation done"
