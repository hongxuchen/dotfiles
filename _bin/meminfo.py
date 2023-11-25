#!/usr/bin/env python3

import psutil


def bytes2human(n):
    # http://code.activestate.com/recipes/578019
    # >>> bytes2human(10000)
    # '9.8K'
    # >>> bytes2human(100001221)
    # '95.4M'
    symbols = ("K", "M", "G", "T", "P", "E", "Z", "Y")
    prefix = {}
    for i, s in enumerate(symbols):
        prefix[s] = 1 << (i + 1) * 10
    for s in reversed(symbols):
        if n >= prefix[s]:
            value = float(n) / prefix[s]
            return f"{value:.2f}{s}"
    return f"{n}B"


def pprint_ntuple(nt):
    name: str
    for name in nt._fields:
        value = getattr(nt, name)
        if name != "percent":
            value = bytes2human(value)
        print(f"{name.capitalize():10s} : {value:>7}")


def main():
    print("MEMORY\n------")
    pprint_ntuple(psutil.virtual_memory())
    print("\nSWAP\n----")
    pprint_ntuple(psutil.swap_memory())


if __name__ == "__main__":
    main()
