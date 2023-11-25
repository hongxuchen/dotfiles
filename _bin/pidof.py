#!/usr/bin/env python3

import os
import sys

import psutil

intepreters = {
    "node",
    "python",
    "python2",
    "python3",
    "lua",
    "luajit",
    "sh",
    "bash",
    "zsh",
}

template = "{:>10d}: {}"


def is_matched(proc: psutil.Process, pgname: str) -> bool:
    if proc.pid == os.getpid():
        return False
    name = proc.info["name"]  # type: ignore
    if pgname.endswith(name):
        return True
    cmdline: list[str] = proc.info["cmdline"]  # type: ignore
    if cmdline is None or len(cmdline) == 0:
        return False
    cmd = cmdline[0]
    if cmd.endswith(pgname):
        return True
    if cmd in intepreters:
        line: list[str] = cmdline[1:]
        for arg in line:
            if not arg.startswith("-") and arg.endswith(pgname):
                return True
    return False


def pidof(pgname):
    pids: list[str] = []
    for proc in psutil.process_iter(attrs=["name", "cmdline"]):
        if is_matched(proc, pgname):
            pids.append(str(proc.pid))
    return pids


def main():
    if len(sys.argv) != 2:
        sys.exit(f"usage: {__file__} pgname")
    else:
        pgname = sys.argv[1]
    pids = pidof(pgname)
    print(" ".join(pids))


if __name__ == "__main__":
    main()
