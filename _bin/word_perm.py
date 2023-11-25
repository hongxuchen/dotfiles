#!/usr/bin/env python

import sys
from itertools import permutations


def get_words(fname):
    word_set = set()
    try:
        with open(fname) as f:
            for word in f.readlines():
                word = word.lower().strip()
                word_set.add(word)
    except FileNotFoundError:
        print(f"cannot find the dict file: {fname}")
        sys.exit(1)
    return word_set


if len(sys.argv) < 2:
    print(
        f"usage: {sys.argv[0]} [alphabets] ([num], [num], ...)",
        file=sys.stderr,
    )
    sys.exit(1)

lists = [w.lower() for w in sys.argv[1]]
if len(lists) < 3:
    print("invalid lists, length should be >= 3", file=sys.stderr)
    sys.exit(1)
nums = [int(num) for num in sys.argv[2:]]
if len(nums) == 0:
    nums = list(range(3, len(lists) + 1))
words = get_words("/usr/share/dict/american-english")
print(f"lists={lists}, nums={nums}")
for num in nums:
    if num > len(lists):
        print("invalid num, ignoring...", file=sys.stderr)
        continue
    print("{}\tnum={}\t{}".format("=" * 20, num, "=" * 20))
    printed = set()
    for p in permutations(lists, num):
        w = "".join(p)
        if w in words and w not in printed:
            print(w)
            printed.add(w)
