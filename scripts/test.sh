#! /bin/bash

cd ./test/scanner
./scripts/build.sh
./scripts/test.sh
./scripts/clean.sh
cd ../parser
./scripts/test.sh
./scripts/clean.sh
cd ../compiler
./scripts/build.sh > build.log
./scripts/test.sh
./scripts/clean.sh
cd ../../hello_world
./scripts/build.sh > build.log
./cmat.native -c hello_world.cmat hello_world.ll
lli hello_world.ll > hello_world.res
diff -q hello_world.out hello_world.res
./scripts/clean.sh
