#!/usr/bin/env python

from __future__ import print_function

import subprocess
import os
import sys
import platform
from colorama import init, Fore
import apt_pkg

list_cmd = [
    ['pip2', 'freeze', '='],
    ['pip3', 'freeze', '='],
    ['gem', 'list', ' '],
    ['brew', 'list', ' ']
]


def backup(cmd, plt):
    cmd_list = cmd[:2]
    print(Fore.YELLOW, ' '.join(cmd_list), Fore.RESET)
    output = subprocess.check_output(cmd_list)
    outname = cmd[0] + '_' + plt
    with open(outname, 'w') as f:
        for line in output.splitlines():
            element = line.split(cmd[2], 2)[0] + '\n'
            f.write(element)

init()
plt = platform.system()
for l in list_cmd:
    backup(l, plt)


if plt == 'Linux':
    dist = platform.dist()[0]
    if dist in ['debian', 'ubuntu']:
        apt_pkg.init()
        cache = apt_pkg.Cache()
        outfile = 'deb_' + dist
        with open(outfile, 'w') as f:
            for pkg in cache.packages:
                if pkg.current_state == apt_pkg.CURSTATE_INSTALLED:
                    f.write(pkg.name + '\n')
