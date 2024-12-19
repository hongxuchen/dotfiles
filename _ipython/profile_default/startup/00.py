import abc
import argparse
import array
import ast
import atexit
import base64
import binascii
import bisect
import contextlib
import copy
import ctypes
import decimal
import difflib
import filecmp
import fileinput
import fnmatch
import fractions
import functools
import gc
import glob
import grp
import hashlib
import heapq
import inspect
import json
import linecache
import math
import mmap
import multiprocessing
import operator
import os
import platform
import random
import re
import shutil
import signal
import string
import struct
import subprocess
import symtable
import sys
import sysconfig
import tempfile
import textwrap
import threading
import trace
import traceback
import uuid

# import sched
import weakref
from collections import *
from itertools import *
from xml.etree.ElementTree import Element, SubElement, tostring

import time
# import datetime
# import calendar
import locale
# import pytz
# import arrow
# import lxml
# import ply.lex
# import psutil
# import pycparser
# import requests
# import yaml

import shlex
# import zmq

def my_trim_whitespace(s):
    """
    Returns a string that has at most one whitespace
    character between non-whitespace characters.
    >>> trim_whitespace(' hi   there')
    'hi there'
    """
    buffer = ""
    for i, letter in enumerate(s):
        if letter.isspace():
            try:
                if s[i + 1].isspace():
                    continue
            except IndexError:
                pass
        buffer = buffer + letter
    return buffer.strip()


def my_load_clang_binding(plt):
    if plt == "Darwin":
        pass
    elif plt == "Linux":
        env_clang_tool_dir = os.environ.get("MY_CLANG_TOOLS")
        if env_clang_tool_dir:
            clang_pybinding = os.path.join(env_clang_tool_dir, "../bindings/python")
            binding_file = os.path.normpath(
                os.path.join(clang_pybinding, "clang/cindex.py"),
            )
            if os.path.isfile(binding_file):
                sys.path.append(clang_pybinding)
                import clang.cindex


plt = platform.system()
if plt == "Darwin":
    import plistlib
elif plt == "Linux":
    pass
# import elftools
# my_load_clang_binding(plt)
