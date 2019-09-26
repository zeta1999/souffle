#!/bin/bash
if [ -z "$1" ]
then
  echo "Usage: souffle-java-gen.sh <input file .dl>"
else
  echo "Executing: souffle, swig, g++ -std=c++11 -fPIC -c -D__EMBEDDED_SOUFFLE__,  g++ -shared -lstdc++"
  echo "Using user input $1"
  echo "..."

  exe=$(basename $1 .dl)
  dir=$(dirname $1)
  currentpath=$(pwd)/$dir
  


  ./../src/souffle -sjava $1
  cd $dir
  javac *.java
  java -Djava.library.path=$currentpath driver
fi

