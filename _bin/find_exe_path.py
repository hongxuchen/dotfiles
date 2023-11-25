#!/usr/bin/env python

import shutil
import sys
from pathlib import Path

if len(sys.argv) < 2:
    print(f"usage: {sys.argv[0]} NAME")
    sys.exit(1)

name = sys.argv[1]
abspath = shutil.which(name)
if abspath is None:
    print(f"cannot find {name}", file=sys.stderr)
    sys.exit(1)

progpath = Path(abspath)
if progpath.is_symlink():
    progpath = progpath.resolve()
print(progpath)
