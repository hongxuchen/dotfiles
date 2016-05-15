#!/usr/bin/env python

from __future__ import print_function
import os
import sys
import platform
import subprocess

try:
    import argparse
    import colorama
except ImportError as e:
    print(
        "{0},\t use 'pip' to install that package".format(
            e.message),
        file=sys.stderr)
    sys.exit(1)

try:
    from subprocess import DEVNULL
except ImportError:
    DEVNULL = open(os.devnull, 'wb')

# -----------------------------------------------------------------------------

CUR_DIR = os.path.abspath(os.path.dirname(__file__))
BAK_DIR = os.path.join(CUR_DIR, "BAK")
PLT = platform.system()  # Darwin/Linux/FreeBSD
SUPPORTED_PLT = ['Darwin', 'Linux', 'FreeBSD']
if PLT not in SUPPORTED_PLT:
    print("Unknown Platform: {}, supported: {}".format(PLT, SUPPORTED_PLT), file=sys.stderr)
    sys.exit(1)
# FreeBSD /home is a symlink to /usr/home
DST_DIR = os.path.realpath(os.path.expandvars("$HOME"))

VUNDLE_REPO = "https://github.com/gmarik/vundle.git"
VUNDLE_PATH = os.path.join(DST_DIR, '.vim/bundle/vundle')

SUFFIX = '.DOTBAK'
M_SAFE = '*'
M_UNSAFE = '!'
# -----------------------------------------------------------------------------

def _git_get(git_repo, dst):
    if not os.path.isdir(dst):
        cmd_str = "git clone --depth 1 {} {}".format(git_repo, dst)
        subprocess.call(cmd_str.split())

def _config_vim():
    print(
        colorama.Fore.YELLOW,
        'Press [ENTER] to ignore errors (if any) during install',
        colorama.Fore.RESET)
    _git_get(VUNDLE_REPO, VUNDLE_PATH)
    cmd_str = "vim -c BundleInstall -c qa"
    subprocess.call(cmd_str.split())

def _config_emacs():
    cmd_str = "emacs -f save-buffers-kill-emacs"
    subprocess.call(cmd_str.split())

def _yn_choice(message, default='y'):
    choices = 'Y/n' if default.lower() in ('y', 'yes') else 'y/N'
    choice = raw_input("{} ({})".format(message, choices))
    values = ('y', 'yes', '') if default == 'y' else ('y', 'yes')
    return choice.strip().lower() in values

def _get_src_files(root):
    src_files = []
    plt_root = os.path.join(root, PLT)
    for r in [root, plt_root]:
        for src in os.listdir(r):
            if src.startswith('_'):
                src_files.append(os.path.join(r, src))
    return src_files

def _ensure_bakdir(bakdir):
    if not os.path.exists(bakdir):
        print("Backup dir [{}] doesn't exist".format(bakdir), file=sys.stderr)
        choice = _yn_choice("Would you like to create it?")
        if choice:
            try:
                os.makedirs(bakdir)
            except os.error as e:
                print(e, file=sys.stderr)
                sys.exit(1)
        else:
            print("Backup aborted, exit", file=sys.stderr)
            sys.exit(1)
    print("backup dir is: {}".format(bakdir))

#------------------------------------------------------------------------

def parse_args():
    parser = argparse.ArgumentParser(
        description="bootstrap for dot configuration", formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        "-c", "--config",
        dest="config",
        nargs="+",
        choices=["vim", "emacs"],
        help="some special configurations")
    parser.add_argument("-r", "--recover", dest="recover", action="store_true", required=False,
                        help="restore the original dotfiles")
    parser.add_argument("-n", "--dryrun", dest="dryrun", action="store_true", required=False,
                        help="print the effect rather than run it")
    parser.add_argument(
        "-d", "--dir",
        dest="bakdir",
        default=BAK_DIR,
        required=False,
        help="restore directory")
    return parser.parse_args()

class FileInfo(object):
    def __init__(self, src, dst, bak):
        self.src = src
        self.dst = dst
        self.bak = bak

    @classmethod
    def apply(cls, src, bakdir):
        src_name = os.path.basename(src)
        dst = os.path.join(DST_DIR, '.' + src_name[1:])  # replace '_' with '.'
        bak = os.path.join(bakdir, os.path.basename(dst) + SUFFIX)
        return cls(src, dst, bak)

def process_all(args):
    _ensure_bakdir(args.backdir)
    src_files = _get_src_files(CUR_DIR)
    for src in src_files:
        src_dir, src_name = os.path.split(src)
        dst = os.path.join(DST_DIR, '.' + src_name[1:])  # replace '_' with '.'
        rel_src = os.path.relpath(src, DST_DIR)
        bak_dst = os.path.join(args.bakdir, os.path.basename(dst) + SUFFIX)
        if not args.recover:
            dst_exist = os.path.exists(dst) or os.path.islink(dst)
            if args.dryrun:
                if dst_exist:
                    mark = M_UNSAFE
                else:
                    mark = M_SAFE
                print("{m} {0:<50} {1:<50}".format(dst, rel_src, m=mark))
            else:
                if dst_exist:
                    print("{0:<50} {1:<50}".format(dst, bak_dst), file=sys.stderr)
                    os.rename(dst, bak_dst)
                os.symlink(rel_src, dst)
        else:
            bak_exist = os.path.exists(bak_dst) or os.path.islink(bak_dst)
            if args.dryrun:
                if not bak_exist:
                    mark = M_UNSAFE
                else:
                    mark = M_SAFE
                print("{m} {0:<50} {1:<50}".format(bak_dst, dst, m=mark))
            else:
                try:
                    os.unlink(dst)
                except OSError:
                    pass
                if bak_exist:
                    os.rename(bak_dst, dst)
                else:
                    print("no backup for: {0}".format(dst))

def main():
    colorama.init()
    args = parse_args()
    if args.config:
        for config in set(args.config):
            eval('_config_' + config)()
    else:
        process_all()

if __name__ == '__main__':
    main()
