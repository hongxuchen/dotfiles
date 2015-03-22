#!/usr/bin/env python

#/usr/bin/env python

import subprocess
import os
import sys
import zipfile
import pycurl

REPO = 'git@github.com:facebook/folly.git'
GTEST_ZIP = 'https://googletest.googlecode.com/files/gtest-1.7.0.zip'
root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), 'folly'))
src_dir = os.path.join(root_dir, 'folly')
test_dir = os.path.join(src_dir, 'test')
zip_name = GTEST_ZIP.split('/')[-1]
gtest_name = os.path.splitext(zip_name)[0]
gtest_dir = os.path.join(test_dir, gtest_name)
print('root_dir: ' + root_dir)
print('src_dir:' + src_dir)
print('gtest_dir: ' + gtest_dir)


def my_call(cmd):
    print(cmd)
    rc = subprocess.call(cmd.split())
    if rc != 0:
        sys.exit(1)

def my_cd(directory):
    print(directory)
    os.chdir(directory)

if not os.path.isdir(root_dir):
    cmd = 'git clone --depth 1 {} {}'.format(REPO, root_dir)
    my_call(cmd)

my_cd(root_dir)
cmd = 'git-up'
my_call(cmd)


if not os.path.isdir(gtest_dir):
    cmd = 'curl {} -o {}'.format(GTEST_ZIP, zip_name)
    my_call(cmd)
    cmd = 'unzip {}'.format(zip_name)
    my_call(cmd)
    os.rename(gtest_name, gtest_dir)

# compile
my_cd(src_dir)

os.environ['CC'] = 'gcc'
os.environ['CXX'] = 'g++'

cmdlist = [
    'autoreconf -fiv',
    './configure',
    'bear make -j',
    'make check',
    'sudo make install'
]

for cmd in cmdlist:
    my_call(cmd)
