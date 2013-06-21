#!/usr/bin/python
import optparse
from optparse import OptionParser
def main():
    parser = OptionParser(usage="%prog [options] arg",version="%prog 1.0.0")
    parser.add_option("-f", "--file", dest="filename",action="store", help="read data from FILE",metavar="FILE")
    parser.add_option("-v", "--verbose", action="store_false", dest="verbose",default=False,help="output status message")
    parser.add_option("-q", "--quiet",default=True, action="store_true", dest="default",help="don't output status message")
    group = optparse.OptionGroup(parser, "Tips",
            "Caution: use these options at your own risk."
            "It is believed that some of them bite.")
    group.add_option("-g", action="store_true", help="generate undesirable result")
    group.add_option("--force", action="store_true", help="force to take action")
    parser.add_option_group(group)
    (options, args) = parser.parse_args()
    if len(args) == 1:
        parser.error("incorrect number of args")
    if options.verbose:
        print "reading %s..." % options.filename
    print options.filename
if __name__ == "__main__":
    main()
