#!/usr/bin/env python3

import getopt
import sys


def ascii(x):
    """Determine how to show a byte in ascii."""
    if 32 <= x <= 126:
        return chr(x)
    else:
        return '.'


def hexdump(f, width=16, verbose=0, start=0):
    pos = 0
    ascmap = [ascii(x) for x in range(256)]
    lastbuf = ''
    lastline = ''
    n_start_len = 0

    if width > 4:
        space_col = width // 2
    else:
        space_col = -1

    hexwidth = 3 * width
    if space_col != -1:
        hexwidth += 1

    if start:
        f.seek(start)
        pos = start

    while 1:
        buf = f.read(width)

        length = len(buf)
        if length == 0:
            if n_start_len:
                if n_start_len > 1:
                    print(("* %d" % (n_start_len - 1)))
                print(lastline)
            return

        bShowBuf = 1

        if not verbose and buf == lastbuf:
            n_start_len += 1
            bShowBuf = 0
        else:
            if n_start_len:
                if n_start_len == 1:
                    print(lastline)
                else:
                    print("* %d" % n_start_len)
            n_start_len = 0

        hex_str = ""
        asc = ""
        for i in range(length):
            c = buf[i]
            if i == space_col:
                hex_str = hex_str + " "
            hex_str = hex_str + ("%02x" % c) + " "
            asc = asc + ascmap[c]
            COLOR_CYAN = '\033[92m'
            COLOR_RESET = '\033[0m'
        line = "%s%06x%s: %-*s %s" \
               % (COLOR_CYAN, pos, COLOR_RESET, hexwidth, hex_str, asc)

        if bShowBuf:
            print(line)

        pos = pos + length
        lastbuf = buf
        lastline = line


def main(args):

    def usage():
        for l in [
            "hexdump: display data in hex",
            "hexdump [opts] [file ...]",
            "opts:",
            " -s offset   start dumping from this offset",
            " -v          show all data (else collapse duplicate lines)",
            " -w width    show data this many bytes at a time (default 16)",
        ]:
                print(l)
        sys.exit()

    try:
        opts, args = getopt.getopt(args, "vw:s:")
    except getopt.GetoptError:
        # print help information and exit:
        usage()

    options = {}

    for o, a in opts:
        if o == '-s':
            start = eval(a)
            if not isinstance(start, int) or start < 0:
                usage()
            options['start'] = start
        elif o == '-v':
            options['verbose'] = 1
        elif o == '-w':
            width = eval(a)
            if not isinstance(width, int) or not (1 <= width <= 100):
                usage()
            options['width'] = width
        else:
            usage()

    # Read stdin if no files were named, otherwise read each named file

    if len(args) == 0:
        hexdump(sys.stdin, **options)
    else:
        for name in args:
            try:
                f = open(name, "rb")
            except IOError:
                print("Couldn't open %s" % name, file=sys.stderr)
                continue
            hexdump(f, **options)
            f.close()

if __name__ == '__main__':
    try:
        main(sys.argv[1:])
    except KeyboardInterrupt:
        print('\n-- interrupted --')
    except IOError:
        print('\n-- broken pipe --')
