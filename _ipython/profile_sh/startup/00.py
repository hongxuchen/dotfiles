#  vim: set ft=python ts=8 sw=4 tw=0 et :
from __future__ import print_function
import atexit
import os
import sys
import linecache
import mmap
import contextlib
import multiprocessing
import subprocess
import socket
import SocketServer
import re
import shutil
import filecmp
import hashlib
import time
import platform
import grp
# import pwd # conflict with magic %pwd
import pipes
import traceback
import trace
import abc
import sysconfig
import Cookie
import uuid
import shlex
import tempfile
import zmq
import gc
import json
import lxml
import symtable
import string
import difflib
import compiler
import pycparser
import ply.lex
import getopt
import Levenshtein
import leveldb
import decimal
import struct
import threading
import Queue
import copy
import processing
import operator
import yaml
import sqlalchemy
import inspect
import math
import ast
import psutil
# import pstats
import binascii
import array
import datetime
import calendar
import heapq
import bisect
import random
import sched
import weakref
import decimal
import fractions
import functools
import base64
import mailbox
import email.utils
import codecs
import locale
import ctypes
import textwrap
import robotparser
import urlparse
import hmac
import fileinput
import fnmatch
import dircache
import bz2
import gzip
import tarfile
import zipfile
import zipimport
import zlib

from xml.etree.ElementTree import Element, SubElement, tostring

from operator import *
from itertools import *
from collections import *
from cStringIO import StringIO

from glob import *
from argparse import *
from cffi import *

def my_trim_whitespace(s):
    """
    Returns a string that has at most one whitespace
    character between non-whitespace characters.
    >>> trim_whitespace(' hi   there')
    'hi there'
    """
    buffer = ''
    for i, letter in enumerate(s):
        if letter.isspace():
            try:
                if s[i + 1].isspace():
                    continue
            except IndexError:
                pass
        buffer = buffer + letter
    return buffer.strip()

def my_load_clang_binding(plt ):
    if plt == 'Darwin':
        pass
        # sys.path.append('/usr/local/Cellar/llvm/3.5.0/lib/python2.7/site-packages')
    elif plt == 'Linux':
        env_clang_tool_dir=os.environ.get('MY_CLANG_TOOLS')
        if env_clang_tool_dir:
            clang_pybinding=os.path.join(env_clang_tool_dir, '..', 'bindings/python')
            binding_file = os.path.normpath(os.path.join(clang_pybinding, 'clang/cindex.py'))
            if os.path.isfile(binding_file):
                sys.path.append(clang_pybinding)
                import clang.cindex

plt = platform.system()
if plt == 'Darwin':
    import appscript
    import plistlib
    import EasyDialogs
    import macholib
    import ds_store
elif plt == 'Linux':
    pass
# import elftools
my_load_clang_binding(plt)
