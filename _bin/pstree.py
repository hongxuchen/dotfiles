#!/usr/bin/env python3

import collections
import sys

import psutil


def print_tree(parent, tree, indent=""):
    try:
        name = psutil.Process(parent).name()
    except psutil.Error:
        name = "?"
    print(parent, name)
    if parent not in tree:
        return
    children = tree[parent][:-1]
    for child in children:
        sys.stdout.write(indent + "|- ")
        print_tree(child, tree, indent + "| ")
    child = tree[parent][-1]
    sys.stdout.write(indent + "`_ ")
    print_tree(child, tree, indent + "  ")


def main():
    # construct a dict where 'values' are all the processes
    # having 'key' as their parent
    tree = collections.defaultdict(list)
    for p in psutil.process_iter():
        try:
            tree[p.ppid()].append(p.pid)
        except (psutil.NoSuchProcess, psutil.ZombieProcess):
            pass
    # on systems supporting PID 0, PID 0's parent is usually 0
    if 0 in tree and 0 in tree[0]:
        tree[0].remove(0)
    print_tree(min(tree), tree)


if __name__ == "__main__":
    main()
