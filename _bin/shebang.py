#!/usr/bin/env python

from __future__ import print_function
from tempfile import mkstemp
import os
import shutil
import stat
from argparse import *
import sys
import colorama


def change_shebang(fpath, target, chmod):
    tmp_fh, tmp_file = mkstemp()
    old_file = open(fpath, 'r')
    new_file = open(tmp_file, 'w')
    old_first_line = old_file.readline()
    new_first_line = "#!/usr/bin/env " + target + "\n"
    if not old_first_line.startswith("#!"):
        new_first_line = new_first_line + "\n" + old_first_line
    new_file.write(new_first_line)
    for line in old_file:
        new_file.write(line)
    new_file.close()
    os.close(tmp_fh)
    old_file.close()
    shutil.move(tmp_file, fpath)
    if chmod:
        os.chmod(fpath, os.stat(fpath).st_mode | stat.S_IEXEC)

if __name__ == '__main__':
    parser = ArgumentParser(description="A snippet to add or change shebang")
    parser.add_argument(
        "-t,",
        "--target",
        metavar="EXE",
        default='python',
        required=False,
        help="the target interpreter(default: python)")
    parser.add_argument(
        "file_list",
        metavar="FILE",
        nargs="+",
        action="store",
        help="FILEs to be processed")
    parser.add_argument(
        "-x",
        action="store_false",
        required=False,
        help="also change FILEs to be executable(default: True, when specified with no arg:False)")
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="print verbose info")
    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(1)
    args = parser.parse_args()
    if args.file_list is None:
        print("file not specified", file=sys.stderr)
        sys.exit(1)
    if args.verbose:
        colorama.init()
        print(
            "target: {2} {0}\n{3}files:\n\t{2}{1}".format(
                args.target,
                args.file_list,
                colorama.Fore.YELLOW,
                colorama.Fore.RESET))
    for f in args.file_list:
        change_shebang(f, args.target, args.x)
