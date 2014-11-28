#!/usr/bin/env python

from __future__ import print_function
import sys
import subprocess
from colorama import *

init()

src=sys.argv[1]
src_prefix, src_suffix = tuple(src.rsplit('.', 1))
ir=src_prefix+'.ll'

if src_suffix in ('.cc', '.cpp', 'cxx'):
    compiler='clang++'
elif src_suffix in ('.c'):
    compiler='clang'
else:
    print("should be c/c++ file", file=sys.stderr)
    exit(1)

if compiler=='clang':
    standard='-std=c99'
elif compiler=='clang++':
    standard='-std=c++11'

opt_pass=['-mem2reg']

p1_call_str='{0} {1} -emit-llvm -S {2} -o - '.format(compiler, standard, src)
p2_call_str='opt -S -o {0} '.format(ir) + ' '.join(opt_pass)
print(Fore.MAGENTA, p1_call_str)
print(Fore.MAGENTA, p2_call_str)

try:
    p1 = subprocess.Popen(p1_call_str.split(' '), stdout=subprocess.PIPE)
    p2 = subprocess.Popen(p2_call_str.split(' '), stdin=p1.stdout, stdout=subprocess.PIPE)
except Exception as e:
    print(Fore.RED, e, file=sys.stderr)
    exit(1)
