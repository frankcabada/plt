#/usr/bin/sh
read -p "Press [Enter] key to start Hello World Demo"
cd hello_world

echo "Compiling source"
./scripts/build.sh > build.log
echo "Compilation complete"

read -p "Press [Enter] key to compile hello world"
cat hello_world.cmat | ./cmat.native -a

echo "Cleaning up"
./scripts/clean.sh
echo "Presentation done"
