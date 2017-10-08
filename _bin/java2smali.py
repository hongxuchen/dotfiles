#!/usr/bin/env python3

from __future__ import print_function

import os
import shutil
import subprocess

version = "1.6"

name = "_java2smali"

dex_type = "apk"


def require(cond, msg):
    if not cond:
        raise Exception(msg)


def run(cmd):
    print("=> {}".format(cmd))
    subprocess.call(cmd.split())


def java2jar(d):
    java_files = [f for f in os.listdir(d) if f.endswith(".java")]
    for f in java_files:
        cmd = "javac -source {} -target {} {}".format(version, version, f)
        run(cmd)
    class_files = [f for f in os.listdir(d) if f.endswith(".class")]
    jar_file = name + ".jar"
    cmd = "jar cvf {} {}".format(jar_file, " ".join(class_files))
    run(cmd)
    require(os.path.exists(jar_file), "{} not generated".format(jar_file))
    for f in class_files:
        os.remove(f)
    return jar_file


def jar2smali(jar_file):
    dex_file = name + "." + dex_type
    cmd = "dx --dex --output={} {}".format(dex_file, jar_file)
    run(cmd)
    require(os.path.exists(dex_file), "{} not generated".format(dex_file))
    os.remove(jar_file)
    cmd = "apktool -f d {}".format(dex_file)
    run(cmd)
    os.remove(dex_file)


def tear_up():
    shutil.rmtree(name, ignore_errors=True)


def java2smali(d):
    tear_up()
    jar_file = java2jar(d)
    jar2smali(jar_file)


if __name__ == '__main__':
    java2smali(os.curdir)
