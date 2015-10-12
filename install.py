#!/usr/bin/env python

from __future__ import print_function
import os
import sys
import platform
import subprocess
import colorama

try:
    import argparse
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


def _find_files(d, files):
    for src in os.listdir(d):
        if src.startswith('_'):
            files.append(os.path.join(d, src))


CUR_DIR = os.path.abspath(os.path.dirname(__file__))
BAK_DIR = os.path.join(CUR_DIR, "BAK")
PLT = platform.system()  # Darwin/Linux
# FreeBSD /home is a symlink to /usr/home
DST_DIR = os.path.realpath(os.path.expandvars("$HOME"))

VUNDLE_REPO = "https://github.com/gmarik/vundle.git"
VUNDLE_PATH = os.path.join(DST_DIR, '.vim/bundle/vundle')


SUFFIX = '.DOTBAK'
M_SAFE = '*'
M_UNSAFE = '!'


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

# ----------------------------------------------------------------------------
colorama.init()
parser = argparse.ArgumentParser(
    description="install scripts for all the dotfiles")
parser.add_argument(
    "--config",
    dest="config",
    nargs="+",
    choices=[
        "vim", "emacs"],
    help="some special configurations")
parser.add_argument("--recover", dest="recover", action="store_true", required=False,
                    help="restore the original dotfiles")
parser.add_argument("-n", dest="dryrun", action="store_true", required=False,
                    help="do not actually run, only print the effect")
parser.add_argument(
    "-d",
    dest="bakdir",
    default=BAK_DIR,
    required=False,
    help="restore directory, default:BAK dir(the same directory as this script)")

args = parser.parse_args()

if args.config:
    for config in args.config:
        eval('_config_' + config)()
    sys.exit(0)

if not os.path.exists(args.bakdir):
    def yn_choice(message, default='y'):
        choices = 'Y/n' if default.lower() in ('y', 'yes') else 'y/N'
        choice = raw_input("{} ({}) " % (message, choices))
        values = ('y', 'yes', '') if default == 'y' else ('y', 'yes')
        return choice.strip().lower() in values
    print("Backup dir [{}] doesn't exist".format(args.bakdir), file=sys.stderr)
    choice = yn_choice("Would you like to create it?")
    if choice is True:
        try:
            os.makedirs(args.bakdir)
        except os.error as e:
            print(e)
            sys.exit(1)
    else:
        print("Backup aborted, exit", file=sys.stderr)
        sys.exit(1)
print("backup dir is: " + args.bakdir)

src_files = []
_find_files(CUR_DIR, src_files)
plt_dir = os.path.join(CUR_DIR, PLT)
_find_files(plt_dir, src_files)
for src in src_files:
    src_dir, src_name = os.path.split(src)
    dst = os.path.join(DST_DIR, '.' + src_name[1:])
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
