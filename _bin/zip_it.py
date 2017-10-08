#!/usr/bin/env python3

import zipfile
import sys

encoding = 'gb2312'

if len(sys.argv) < 2:
    print('{} zipfilename'.format(sys.argv[0]))
else:
    f = zipfile.ZipFile(sys.argv[1])
    nlist = f.namelist()
    for n in nlist:
        m = unicode(n, encoding).encode('utf8')
        file(m, 'wb').write(f.read(n))
    f.close()
