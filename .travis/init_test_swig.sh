#!/bin/bash


# Enable debugging and logging of shell operations
# that are executed.
set -e
set -x

# create configure files
./bootstrap
./configure --enable-swig


set +e
set +x
