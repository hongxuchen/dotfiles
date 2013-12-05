#!/bin/bash
src=$1
tmp=${1%.*}_tmp.ll
ir=${1%.*}.ll
remain=${*:2}
clang -std=c99 -S -emit-llvm $src $remain -o - | opt -mem2reg -S -o $ir
# opt -mem2reg $tmp -S -o $ir
