#!/usr/bin/python3

import sys
from tempfile import mkstemp
from shutil import move
import os
import re
import stat


def change_shebang(file_path, old, new):
    fh, abs_path = mkstemp()
    old_file = open(file_path, 'r')
    first_line = old_file.readline()
    if first_line.startswith('#!'):
        new_file = open(abs_path, 'w')
        new_line = old.sub(new, first_line)
        new_file.write(new_line)
        for line in old_file:
            new_file.write(line)
        new_file.close()
        os.close(fh)
        old_file.close()
        move(abs_path, file_path)
        os.chmod( file_path, os.stat(file_path).st_mode | stat.S_IEXEC )

if __name__ == '__main__':
    assert(len(sys.argv) > 1)
    pat = re.compile(r'\bpython(2|(2\.[0-9]))?\b')
    change_shebang(sys.argv[1], pat, 'python3')
