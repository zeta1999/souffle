#!/bin/bash


# Enable debugging and logging of shell operations
# that are executed.
set -e
set -x

# configure project

# create configure files
./bootstrap
./configure CC=gcc-9  CPPFLAGS='-I /usr/local/opt/gcc@9/include' CXX=g++-9  --enable-debug
make -j2


set +e
set +x
