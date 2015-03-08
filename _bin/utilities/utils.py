import os
import sys


def getpass(prompt="Password: "):
    import termios
    import sys
    fd = sys.stdin.fileno()
    old = termios.tcgetattr(fd)
    new = termios.tcgetattr(fd)
    new[3] = new[3] & ~termios.ECHO          # lflags
    try:
        termios.tcsetattr(fd, termios.TCSADRAIN, new)
        passwd = raw_input(prompt)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old)
    return passwd


def path_import(path):
    '''import with "path"'''
    d, f = os.path.split(path)
    if d not in sys.path:
        sys.path[0:0] = [d]
        ret = __import__(os.path.splitext(f)[0])
        del sys.path[0]
        return ret


def filesize(size):
    '''transfer digits to the form of xxKiB'''
    units = 'KMGT'
    left = abs(size)
    unit = -1
    while left > 1100 and unit < 3:
        left = left / 1024
        unit += 1
    if unit == -1:
        return '%dB' % size
    else:
        if size < 0:
            left = -left
        return '%.1f%siB' % (left, units[unit])


def input_t(timeout, prompt=''):
    '''input with timeout (None is returned), implemented with selected()'''
    from select import select
    sys.stdout.write(prompt)
    sys.stdout.flush()
    if select([sys.stdin.fileno()], [], [], timeout)[0]:
        return input()


def getchar(prompt, hidden=False, end='\n'):
    '''read one char'''
    import termios
    sys.stdout.write(prompt)
    sys.stdout.flush()
    fd = sys.stdin.fileno()

    if os.isatty(fd):
        old = termios.tcgetattr(fd)
        new = termios.tcgetattr(fd)
        if hidden:
            new[3] = new[3] & ~termios.ICANON & ~termios.ECHO
        else:
            new[3] = new[3] & ~termios.ICANON
        new[6][termios.VMIN] = 1
        new[6][termios.VTIME] = 0
        try:
            termios.tcsetattr(fd, termios.TCSANOW, new)
            termios.tcsendbreak(fd, 0)
            ch = os.read(fd, 7)
        finally:
            termios.tcsetattr(fd, termios.TCSAFLUSH, old)
    else:
        ch = os.read(fd, 7)

    sys.stdout.write(end)
    return(ch.decode())


def loadso(fname):
    '''wrapper for ctypes.CDLL'''
    from ctypes import CDLL
    for d in sys.path:
        p = os.path.join(d, fname)
        if os.path.exists(p):
            return CDLL(p)
    raise ImportError('%s not found' % fname)
