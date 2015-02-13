#!/usr/bin/env python
# vim:fileencoding=utf-8

from itertools import izip_longest

text = u'一去二三里\n烟村四五家\n亭台六七座\n八九十支花'

print '\n'.join(map(lambda x: ''.join(reversed(x)), izip_longest(*text.split('\n'), fillvalue=u'　')))
