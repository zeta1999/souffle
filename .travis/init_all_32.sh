#!/bin/bash


# Enable debugging and logging of shell operations
# that are executed.
set -e
set -x

JOBS=$(nproc || sysctl -n hw.ncpu || echo 2)

# configure project

# Ensure we have the tags before attempting to use them
# Avoids issues with shallow clones not finding tags.
git fetch --tags --unshallow origin master || true
echo -n "Version: "
git describe --tags --abbrev=0 --always

# create configure files
./bootstrap
./configure $1
make -j$JOBS

set +e
set +x
