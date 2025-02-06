#!/usr/bin/env python

import argparse
import os
import shutil
import stat
import sys
from tempfile import mkstemp

import colorama

ext_app_dict = {
    ".py": "python",
    ".pl": "perl",
    ".sh": "bash",
    ".zsh": "zsh",
    ".lua": "lua",
    ".rb": "ruby",
    ".ml": "ocaml",
    ".go": "go",
    ".nu": "nu",
}


def gen_first_line(target: str):
    if target == "go":
        return '/// 2>/dev/null ; gorun "$0" "$@" ; exit $?'
    else:
        return "#!/usr/bin/env " + target + "\n"


def change_shebang(fpath: str, target: str, chmod):
    if not os.path.exists(fpath):
        print(f"{fpath} not exists", file=sys.stderr)
        sys.exit(1)
    if target is None:
        fext = os.path.splitext(fpath)[1]
        if fext == "":
            print(
                f"{colorama.Fore.YELLOW}no extension found for [{fpath}], treated as a shell script{colorama.Fore.RESET}",
            )
            fext = ".sh"
        try:
            target = ext_app_dict[fext]
        except KeyError as e:
            print(f"No target interpreter for {e} files", file=sys.stderr)
            sys.exit(1)
    tmp_fh, tmp_file = mkstemp()
    old_file = open(fpath)
    new_file = open(tmp_file, "w")
    old_first_line = old_file.readline()
    new_first_line = gen_first_line(target)
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


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="A snippet to add or change shebang",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "-t",
        "--target",
        metavar="EXE",
        required=False,
        help="the target interpreter; if not specified, guess from file extension",
    )
    parser.add_argument(
        "file_list",
        metavar="FILE",
        nargs="+",
        action="store",
        help="FILEs to be processed",
    )
    parser.add_argument(
        "-x",
        action="store_false",
        required=False,
        help="also change FILEs to be executable",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="print verbose info",
    )
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
            f"target: {colorama.Fore.YELLOW} {args.target}\n{colorama.Fore.RESET}files:\n\t{colorama.Fore.YELLOW}{args.file_list}",
        )
    for f in args.file_list:
        change_shebang(f, args.target, args.x)
