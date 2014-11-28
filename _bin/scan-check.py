#!/usr/bin/python3

import sys
import os

def which(program):

    def is_exe(fpath):
        if not os.path.isfile(fpath) or not os.access(fpath, os.X_OK):
            return False
        return True

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file
    return None

for exe_name in ["scan-build", "scan-view", "clang-3.6"]:
    if which(exe_name) is None:
        print("{:s} cannot be found".format(exe_name))
        sys.exit(1)
clang_exe = which("clang") + " "
scan_build_exe = which("scan-build")
cmd_str = scan_build_exe + " --use-analyzer " + clang_exe + \
    " -load-plugin ~/repos/snippets_clang/build/bin/libplugin.so -disable-checker core.DivideZero -enable-checker hello.DZChecker -enable-checker security.awr.NetworkTaint"
for argv in sys.argv[1:]:
    cmd_str += (" " + argv)
print(cmd_str)
# print(sh.perl(cmd_str.split()))
