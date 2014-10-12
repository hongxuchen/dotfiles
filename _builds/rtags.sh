#!/bin/bash

BASEDIR="`dirname $0`"/repos
mkdir -p repos
cd $BASEDIR
SRC=$(pwd)/rtags
BUILD=$SRC/build
ELISP=$HOME/.emacs.d//rtags

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
C_INCLUDE_PATH=
CPLUS_INCLUDE_PATH=
LD_LIBRARY_PATH=
export PATH=/usr/bin:$PATH
export CC=gcc
export CXX=g++
# cmake -GNnja -DCLANG_ROOT=${INSTALL_DIR}  $SRC  && ninja
cmake -GNinja $SRC  && ninja
# cmake $SRC && make -j$(nproc)

ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/g++
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/gcc
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/clang++
ln -sf $SRC/bin/gcc-rtags-wrapper.sh ~/.bin/clang

ln -sf $SRC $ELISP
