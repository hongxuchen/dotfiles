#!/usr/bin/env python3

import sys
from urllib.request import urlretrieve
import subprocess

web_prefix="https://github.com"
raw_prefix="https://raw.githubusercontent.com"

if len(sys.argv) < 2:
    print("usage: {:s} github_url".format(sys.argv[0]))
    exit(1)
url = sys.argv[1]
if web_prefix not in url:
    print("should be github url")
    sys.exit(1)
raw_url = url.replace(web_prefix, raw_prefix).replace('blob/', '')
print("downloading from: " + raw_url)
idx = url.rfind('/')
filename = url[idx+1:]
cmd_str = 'curl {0} -o {1}'.format(raw_url, filename)
subprocess.call(cmd_str.split(' '))
