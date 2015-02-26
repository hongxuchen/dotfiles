#!/usr/bin/env python

from __future__ import print_function
import subprocess

def dep_info(installed):
    for i in installed:
        deps_cmd = deps.format(i)
        try:
            depends = subprocess.check_output(deps_cmd.split())
            depends_list = depends.split()
            new_list = [e for e in depends_list if e in installed]
            yield i, new_list
        except (KeyboardInterrupt, SystemExit):
            import sys
            sys.exit(1)
        except:
            pass

if __name__ == '__main__':
    cmd = 'brew list'
    installed = subprocess.check_output(cmd.split()).split()
    deps = 'brew uses {}'
    for i, e in enumerate(dep_info(installed)):
        print('{:3} {:50} {}'.format(i, e[0], e[1]))
