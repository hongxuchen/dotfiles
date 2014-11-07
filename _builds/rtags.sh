#!/bin/bash

BASEDIR=$(dirname $0)
cd $BASEDIR
SRC=$(pwd)/rtags
BUILD=$SRC/build
ELISP=$HOME/.emacs.d/config/rtags

rm -f ~/.bin/clang
rm -f ~/.bin/clang++
rm -f ~/.bin/gcc
rm -f ~/.bin/g++

if ! [ -d $SRC ]; then
    git clone --recursive git@github.com:Andersbakken/rtags.git  $SRC
else
    cd $SRC
    git pull --all
    git submodule update --recursive
fi

rm $ELISP
git checkout master

rm -rf $BUILD && mkdir -p $BUILD && cd $BUILD
C_INCLUDE_PATH=
CPLUS_INCLUDE_PATH=
LD_LIBRARY_PATH=
export PATH=/usr/bin:$PATH
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -GNinja $SRC  
ninja

ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/g++
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/gcc
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/clang++
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/clang

ln -sf $SRC $ELISP
