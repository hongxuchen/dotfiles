#!/usr/bin/python3

# for small python scripts, simply load them into memory then write back

import sys
from tempfile import mkstemp
import os
import shutil
import re
import stat

def change_shebang(file_path, old, new):
    fh, abs_path = mkstemp()
    old_file = open(file_path, 'r')
    new_file = open(abs_path, 'w')
    first_line = old_file.readline()
    if first_line.startswith('#!'):
        new_line = old.sub(new, first_line)
    else:
        new_line = '#!/usr/bin/python3\n\n' + first_line
    new_file.write(new_line)
    for line in old_file:
        new_file.write(line)
    new_file.close()
    os.close(fh)
    old_file.close()
    shutil.move(abs_path, file_path)
    os.chmod( file_path, os.stat(file_path).st_mode | stat.S_IEXEC )

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("usage: {0} pyfile1 [pyfile2]".format(__file__))
        sys.exit(1)
    pat = re.compile(r'\bpython(2|(2\.[0-9]))?\b')
    pylist = sys.argv[1:]
    change_shebang(sys.argv[1], pat, 'python3')
