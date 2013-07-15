#!/bin/sh
set -ex
BASEDIR="`dirname $0`"
# git clone https://github.com/b4winckler/vim ${BASEDIR}/vim
cd ${BASEDIR}/vim
git checkout master
git pull
./configure --with-features=huge --enable-rubyinterp --enable-pythoninterp --enable-cscope --disable-nls --program-suffix='-chx'
make
sudo checkinstall
