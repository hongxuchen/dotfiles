#!/usr/bin/env python
# PYTHON_ARGCOMPLETE_OK

from __future__ import print_function
from tempfile import mkstemp
import os
import shutil
import stat
from argparse import *
import sys
import colorama
import argcomplete

ext_app_dict = {
    '.py': 'python',
    '.pl': 'perl',
    '.sh': 'bash',
    '.zsh': 'zsh',
    '.rb': 'ruby',
    '.applescript': 'osascript',
    '.ml': 'ocaml'
}


def change_shebang(fpath, target, chmod):
    if not os.path.exists(fpath):
        print('{} not exists'.format(fpath), file=sys.stderr)
        sys.exit(1)
    if target is None:
        fext = os.path.splitext(fpath)[1]
        if fext == '':
            print("{}no extension found for [{}], treated as a shell script{}".format(colorama.Fore.YELLOW, fpath, colorama.Fore.RESET))
            fext = ".sh"
        try:
            target = ext_app_dict[fext]
        except KeyError as e:
            print(
                'No target interpreter for {} files'.format(e),
                file=sys.stderr)
            sys.exit(1)
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
    parser = ArgumentParser(
        description="A snippet to add or change shebang",
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        "-t",
        "--target",
        metavar="EXE",
        required=False,
        help="the target interpreter; if not specified, guess from file extension")
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
        help="also change FILEs to be executable")
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="print verbose info")
    argcomplete.autocomplete(parser)
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
