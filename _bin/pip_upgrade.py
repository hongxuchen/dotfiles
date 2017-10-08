#!/usr/bin/env python3
from __future__ import print_function

import pip
from subprocess import call
from colorama import init, Fore

import sys

if sys.version_info.major == 2:
    PIP = 'pip'
else:
    PIP = 'pip3'

init()

black_list = ['scipy', 'PySide', 'Magic-file-extensions', '-lxc', 'python-apt']
failed_list = []
for dist in pip.get_installed_distributions():
    dist_name = dist.project_name
    if dist_name in black_list or '/usr/lib/pymodules' in dist.location:
        continue
    try:
        # cmd = 'install --upgrade {}'.format(dist_name)
        # rc = pip.main(cmd.split())
        cmd = '{} install --upgrade {}'.format(PIP, dist_name)
        print(Fore.YELLOW, 'installing ', Fore.CYAN, dist_name, Fore.RESET)
        rc = call(cmd.split())
    except Exception as e:
        print(e, file=sys.stderr)
        sys.exit(1)
    if rc != 0:
        failed_list.append(dist_name)

print('=' * 80)
print('\n'.join(failed_list))
