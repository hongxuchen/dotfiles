#!/bin/bash

# llvm
svn co http://llvm.org/svn/llvm-project/llvm/trunk llvm-svn

# clang
cd llvm-svn/tools
svn co http://llvm.org/svn/llvm-project/cfe/trunk clang
cd ../..

# clang extra
cd llvm-svn/tools/clang/tools
svn co http://llvm.org/svn/llvm-project/clang-tools-extra/trunk extra
cd ../../../..

# compiler-rt
# cd llvm-svn/projects
# svn co http://llvm.org/svn/llvm-project/compiler-rt/trunk compiler-rt
# cd ../..
