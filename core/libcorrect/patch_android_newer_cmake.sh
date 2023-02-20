#!/bin/bash
cd `dirname $0`
find . -name 'CMakeLists.txt'| while read i; do  sed -i  "s/OBJECT //g"  $i; done
sed -i 's/VERSION 2.8/VERSION 3.13/g' CMakeLists.txt
