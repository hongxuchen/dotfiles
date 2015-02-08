#!/usr/bin/env python

from __future__ import print_function
import re
import sys
import os
import tempfile

SUFFIX_HEADER = '.hh'
SUFFIX_SOURCE = '.cc'
SUFFIX_HEADER_OLD = '.hpp'
head_pat = '[^:]+\{}'.format(SUFFIX_HEADER_OLD)
pat_str = r'#\s*include\s+(?:"({})"|<({})>)'.format(head_pat, head_pat)
pat = re.compile(pat_str)

def replace_headers(infile):
    outfd, outfile = tempfile.mkstemp()
    with open(infile, 'r+') as in_data:
        with open(outfile, 'w') as out_data:
            for line in in_data:
                m = re.search(pat, line)
                if m:
                    line = line.replace(SUFFIX_HEADER_OLD, SUFFIX_HEADER)
                out_data.write(line)
    os.rename(outfile, infile)

if len(sys.argv) < 2:
    rootdir = os.path.curdir
else:
    rootdir = sys.argv[1]
for root, dirs, files in os.walk(rootdir):
    for f in files:
        if f[-3:] in [SUFFIX_HEADER, SUFFIX_SOURCE]:
            infile = os.path.join(root, f)
            print(infile)
            replace_headers(infile)
