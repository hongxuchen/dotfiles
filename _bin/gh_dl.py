#!/usr/bin/python3

import sys
from urllib.request import urlretrieve

if len(sys.argv) < 2:
    print("usage: {:s} github_url".format(sys.argv[0]))
    exit(1)
url = sys.argv[1]
url = url.replace('blob', 'raw')
idx = url.rfind('/')
filename = url[idx+1:]
urlretrieve(url, filename)
