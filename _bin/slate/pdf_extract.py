#!/usr/bin/python2

from slate import PDF
import sys

if len(sys.argv) != 2:
    print("usage: %s {pdf-file-name}" % sys.argv[0])
else:
    pdf_object = PDF(open(sys.argv[1], 'rb'))
    metadata = pdf_object.metadata[0]
    format_str = '{:<20} {:<15}'
    print(format_str.format("key", "value"))
    for key, value in metadata.items():
        print(format_str.format(key, value))
