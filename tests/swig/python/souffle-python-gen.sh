#!/bin/bash
if [ -z "$1" ]
then
  echo "Usage: Script_Python.sh <input file .dl>"
else
  echo "Using user input $1"
  echo "..."

  exe=$(basename $1 .dl)
  dir=$(dirname $1)

  ./../src/souffle -spython $1
  cd $dir
  python3 driver.py
fi