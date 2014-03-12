#!/bin/bash

BASEDIR="`dirname $0`"/repos
cd $BASEDIR

git clone --depth 1 -b master git://github.com/libgit2/libgit2.git
mkdir libgit2/build
cd libgit2/build
cmake ..
cmake --build .
sudo cmake --build . --target install
cd ../..
