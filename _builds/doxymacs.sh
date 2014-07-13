#!/bin/sh

BASEDIR="`dirname $0`"
cd $BASEDIR
REPOS=$(pwd)/repos
SRC=${REPOS}/doxymacs-1.8.0

if ! [ -d $SRC ]; then
    cd ${REPOS}
    wget ${SRC} http://downloads.sourceforge.net/project/doxymacs/doxymacs/1.8.0/doxymacs-1.8.0.tar.gz 
    tar xvf ${SRC}.tar.gz
    rm ${SRC}.tar.gz
fi

cd ${SRC}

./configure --prefix=$HOME/.emacs.d/site-lisp/doxymacs #--with-default-style=JavaDoc
make
make install
