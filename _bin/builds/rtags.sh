#!/bin/bash

# We'd better use a deb package of libclang/llvm; clang isn't necessary

BASEDIR="`dirname $0`"
cd $BASEDIR
SRC=$(pwd)/rtags
BUILD=$SRC/build
ELISP=$HOME/.emacs.d/elisp/rtags

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

# INSTALL_DIR=$HOME//llvm/install
# PATH=${INSTALL_DIR}/bin:$PATH
rm -rf $BUILD && mkdir -p $BUILD && cd $BUILD
# cmake -GNinja -DCLANG_ROOT=${INSTALL_DIR}  $SRC  && ninja
cmake -GNinja $SRC  && ninja

ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/g++
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/gcc
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/clang++
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/clang

ln -sf $SRC $ELISP
