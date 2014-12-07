#!/usr/bin/env python

from __future__ import print_function
import os
import sys
import platform
try:
    from argparse import *
except ImportError as e:
    print("{0},\t use 'pip' to install that package".format(e.message), file=sys.stderr)
    sys.exit(1)

cur_dir = os.path.dirname(__file__)
plt = platform.platform()


def link_files(bakdir, is_recovery):
    for f in os.listdir(cur_dir):
        if not f.startswith('_'):
            continue
        dotfile = os.path.join(os.path.expanduser("~"), '.'+f[1:])
        print("{:<30} {:50}".format(f, dotfile))


parser = ArgumentParser(description="install scripts for all the dotfiles")
parser.add_argument("-r", dest="recover", action="store_true", required=False, help="restore the original dotfiles")
parser.add_argument("-d", dest="bakdir", default=os.path.expanduser("~"), required=False, help="restore directory, default:$HOME")

args = parser.parse_args()
print("backup dir is: " + args.bakdir)
link_files(args.bakdir, args.recover)