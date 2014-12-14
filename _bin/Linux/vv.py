#!/usr/bin/env python

from __future__ import print_function
import sys
import os
import subprocess

try:
    from subprocess import DEVNULL
except ImportError:
    DEVNULL = open(os.devnull, 'wb')

if len(sys.argv) < 2:
    percent = 10
else:
    arg = sys.argv[1]
    try:
        percent = int(arg)
    except:
        print("error: {} should be an int".format(argv), file=sys.stderr)
        sys.exit(1)

direction = '+'
if percent < 0:
    direction = '-'
    percent = - percent
cmd_str = "amixer set Master {0:d}%{1}".format(percent, direction)
subprocess.Popen(cmd_str.split(), stdout=DEVNULL)
