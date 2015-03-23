#!/usr/bin/python3

import subprocess
import time
import sys

project_list = [
    # "/home/hongxu/RESEARCH/marple/build",
    # "/home/hongxu/RESEARCH/buildbot/coreutils/obj",
    "/home/hongxu/RESEARCH/llvm-git/obj",
    # "/home/hongxu/RESEARCH/snippets_llvm/build",
    "/home/hongxu/RESEARCH/llvm-3.4/obj",
    "/home/hongxu/.builds/rtags/build",
    "/home/hongxu/RESEARCH/klee/obj",
    "/home/hongxu/RESEARCH/uclibc",
    "/home/hongxu/RESEARCH/stp/obj",
    "/home/hongxu/RESEARCH/Z3/src/build",
]

for project in project_list:
    cmd_str = "rc -J {:s}".format(project)
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
