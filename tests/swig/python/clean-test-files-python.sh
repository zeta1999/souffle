#!/bin/bash
if [ -z "$1" ]
then
  echo "Usage: clean-test-files-python.sh <test folder>"
else
  if [ $(ls -l $1 | wc -l) -lt 3 ]
  then
    echo "Folder already cleaned"
  else
    for file in `ls -1 $1 | grep -v .dl$ | grep -v driver.py | grep -v facts | grep -v .facts$`
    do
      rm -rf $1/$file
    done
    echo "Folder cleaned"
  fi
fi