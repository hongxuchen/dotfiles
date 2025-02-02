#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import os
import platform
import shutil
import sys

from typing_extensions import override


class FileInfo(object):
    def __init__(self, src: str, dst: str, bak: str):
        self.src: str = src
        self.dst: str = dst
        self.bak: str = bak

    @classmethod
    # always return an abspath
    def apply(cls, src: str, bakdir: str):
        src_name = os.path.basename(src)
        # replace "_" with "."
        dst = os.path.join(DST_DIR, "." + src_name[1:])
        bak = os.path.join(bakdir, PREFIX + os.path.basename(dst))
        return cls(src, dst, bak)

    @override
    def __repr__(self):
        return f"src={self.src}, dst={self.dst}, bak={self.bak}"


class MLevel(object):
    check = "✔"
    cross = "✗"
    other = "○"


CUR_DIR: str = os.path.abspath(os.path.dirname(__file__))
BAK_DIR: str = os.path.join(CUR_DIR, ".BAK")
PLT: str = platform.system()  # Darwin/Linux
SUPPORTED_PLT = ["Darwin", "Linux"]
if PLT not in SUPPORTED_PLT:
    print(f"Unknown Platform: {PLT}, supported: {SUPPORTED_PLT}", file=sys.stderr)
    sys.exit(1)
DST_DIR = os.path.realpath(os.path.expandvars("$HOME"))

PREFIX = "_bak"

# -----------------------------------------------------------------------------


def _yn_choice(message: str, default: str = "y") -> bool:
    choices = "Y/n" if default.lower() in ("y", "yes") else "y/N"
    choice = input(f"{message} ({choices})")
    values = ("y", "yes", "") if default == "y" else ("y", "yes")
    return choice.strip().lower() in values


def _get_src_files(root: str) -> list[str]:
    src_files: list[str] = []
    plt_root = os.path.join(root, PLT)
    for r in [root, plt_root]:
        for src in os.listdir(r):
            if src.startswith("_"):
                src_files.append(os.path.join(r, src))
    return src_files


def _ensure_bakdir(bakdir: str):
    if not os.path.exists(bakdir):
        print("backup dir [{bakdir}] doesn't exist", file=sys.stderr)
        choice = _yn_choice("Would you like to create it?")
        if choice:
            try:
                os.makedirs(bakdir)
            except os.error as e:
                print(e, file=sys.stderr)
                sys.exit(1)
        else:
            print("backup aborted, exit", file=sys.stderr)
            sys.exit(1)
    print(f"backup dir is: {bakdir}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="bootstrap for dotfiles configuration",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "-r",
        "--recover",
        dest="recover",
        action="store_true",
        required=False,
        help="restore the original dotfiles",
    )
    parser.add_argument(
        "-n",
        "--dryrun",
        dest="dryrun",
        action="store_true",
        required=False,
        help="print the effect rather than run it",
    )
    parser.add_argument(
        "-d",
        "--dir",
        dest="bakdir",
        default=BAK_DIR,
        required=False,
        help="restore directory",
    )
    return parser.parse_args()


# dot: dst->backdir, src->dst(symlink)
# recover: dst->xxx(unlink), backdir->dst


def __dot_mark(finfo: FileInfo) -> str:
    if not os.path.exists(finfo.dst):
        mark = MLevel.check
    elif os.path.islink(finfo.dst):
        mark = MLevel.other
    else:
        mark = MLevel.cross
    return mark


def _dot_dryrun(finfo: FileInfo):
    mark = __dot_mark(finfo)
    rel_src = os.path.relpath(finfo.src, DST_DIR)
    print("{0:<60} ⇒ \t[{m}] {1:<50}".format(rel_src, finfo.dst, m=mark))


def _dot(finfo: FileInfo):
    dst_exist = os.path.exists(finfo.dst) or os.path.islink(finfo.dst)
    if dst_exist:
        print("dst={0:<60} bak={1}".format(finfo.dst, finfo.bak), file=sys.stderr)
        if os.path.islink(finfo.dst):
            os.unlink(finfo.dst)
        else:
            shutil.move(finfo.dst, finfo.bak)
    rel_src = os.path.relpath(finfo.src, DST_DIR)
    os.symlink(rel_src, finfo.dst)


def __recover_mark(finfo: FileInfo) -> str:
    if os.path.exists(finfo.bak) and not os.path.islink(finfo.bak):
        mark = MLevel.check
    else:
        mark = MLevel.cross
    return mark


def _recover_dryrun(finfo: FileInfo):
    mark = __recover_mark(finfo)
    rel_bak = os.path.relpath(finfo.bak, CUR_DIR)
    print("{0:<60} ->\t[{m}] {1:<50}".format(rel_bak, finfo.dst, m=mark))


def _recover(finfo: FileInfo):
    try:
        os.unlink(finfo.dst)
    except OSError as e:
        # dotfile in CUR_DIR may not exists in DST_DIR
        print(f"unlink Error {e}", file=sys.stderr)
    bak_exist = os.path.exists(finfo.bak) and not os.path.islink(finfo.bak)
    if bak_exist:
        os.rename(finfo.bak, finfo.dst)
    else:
        print(f"no backup for: {finfo.dst}", file=sys.stderr)


def dot_driver(args: argparse.Namespace):
    """driver function to deal with different options"""
    if not args.dryrun:
        _ensure_bakdir(args.bakdir)
    src_files = _get_src_files(CUR_DIR)
    if not args.recover:
        fn = _dot_dryrun if args.dryrun else _dot
    else:
        fn = _recover_dryrun if args.dryrun else _recover
    for src in src_files:
        finfo = FileInfo.apply(src, args.bakdir)
        fn(finfo)


def main():
    args = parse_args()
    dot_driver(args)


if __name__ == "__main__":
    main()
