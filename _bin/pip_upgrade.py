#!/usr/bin/env python

import pip
from subprocess import call

black_list = ['scipy', 'PySide', 'Magic-file-extensions', '-lxc', 'python-apt']
failed_list = []
for dist in pip.get_installed_distributions():
    dist_name = dist.project_name
    if dist_name in black_list:
        continue
    rc = call(("pip install --upgrade " + dist_name).split(), shell=False)
    if rc != 0:
        failed_list.append(dist_name)

print('=' * 80)
print('\n'.join(failed_list))
