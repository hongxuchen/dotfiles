#!/bin/bash

BASEDIR="`dirname $0`"
cd $BASEDIR

rm -rf build && mkdir -p build && cd build
cmake -GNinja -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=../llvm/install -DLLVM_BUILD_EXAMPLES=0 -DLLVM_ENABLE_ASSERTIONS=1 -DLLVM_ENABLE_CXX11=1 ../llvm-svn/
# ninja
# ninja install
