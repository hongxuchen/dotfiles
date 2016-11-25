#!/usr/bin/python3

import subprocess
import time
import sys
import os

project_list = [
    "~/RESEARCH/llvm-git/obj",
    "~/RESEARCH/llvm-3.4/obj",
    "~/.builds/rtags/build",
    "~/RESEARCH/klee/obj",
    "~/RESEARCH/uclibc",
    "~/RESEARCH/stp/obj",
    "~/RESEARCH/Z3/src/build",
]

for project in project_list:
    cmd_str = "rc -J {:s}".format(os.path.expanduser(project))
    print(cmd_str)
    try:
        subprocess.call(cmd_str.split(" "))
    except:
        sys.exit(1)
    # time.sleep(3)
sys.exit(0)

for project in project_list:
    if project.endswith("obj"):
        project = project[:-4]
    elif project.endswith("build"):
        project = project[:-6]
    cmd_str = "rc -V {:s}".format(project)
    print(cmd_str)
    subprocess.call(cmd_str.split(" "))
