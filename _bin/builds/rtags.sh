#!/bin/bash

BASEDIR="`dirname $0`"
cd $BASEDIR
SRC=$PWD/rtags

# rm -f ~/.bin/clang
# rm -f ~/.bin/clang++
# rm -f ~/.bin/gcc
# rm -f ~/.bin/g++

# if ! [ -d $SRC ]; then
#     git clone --recursive git@github.com:Andersbakken/rtags.git  $SRC
# else
#     cd $SRC
#     git pull --all
#     git submodule update --recursive
# fi

cd $SRC
git checkout master
rm -rf CMakeCache.txt CMakeFiles
cmake $SRC
make -j 2

ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/g++
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/gcc
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/clang++
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/clang
ln -sf $SRC ~/.emacs.d/elisp/rtags
