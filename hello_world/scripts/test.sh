#!/bin/bash

./cmat.native -c hello_world.cmat hello_world.ll
lli hello_world.ll > hello_world.res
diff hello_world.res hello_world.out > /dev/null