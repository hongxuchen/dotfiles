#!/usr/bin/env python

from __future__ import print_function
import subprocess

cmd = 'brew list'
installed = subprocess.check_output(cmd.split()).split()
deps = 'brew uses {}'
d = dict()
for i in installed:
    deps_cmd = deps.format(i)
    depends = subprocess.check_output(deps_cmd.split())
    if depends == '':
        print(i)
    d[i] = depends
