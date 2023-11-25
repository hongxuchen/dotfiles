#!/usr/bin/env python3

import collections

import psutil


def dump():
    procs = collections.defaultdict(list)
    processors = psutil.process_iter(attrs=["name", "cpu_num"])
    for p in processors:
        procs[p.info["cpu_num"]].append(p.info["name"])  # pyright ignore
    cpu_num = psutil.cpu_count()
    for i in range(cpu_num):
        print(f"{i}: {procs[i]}")


dump()
