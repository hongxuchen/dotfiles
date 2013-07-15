#!/bin/sh
build_emacs(){
    set -ex
    BASEDIR="`dirname $0`"
    cd ${BASEDIR}/emacs-24.3
    ./configure --disable-largefile --program-suffix='-chx' --without-xpm --without-jpeg --without-tiff --without-gif --without-png --without-imagemagick  --without-gconf --without-selinux --without-gsettings --without-gpm --without-makeinfo --with-x-toolkit=no CC=/usr/bin/clang CXX=/usr/bin/clang++ CXXFLAGS=-O3 CFLAGS=-O3
    make -j
    sudo checkinstall
    cd ${BASEDIR}
}
#--without-toolkit-scroll-bars  --without-xaw3d
build_emacs 2>&1 |tee BUILD_LOG
