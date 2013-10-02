#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Pyget is a free downloader for multiprocess & resume broken downloads of files
base on libcurl.

Synopsis:
    ./pyget.py [-w arg] [URL | FILE]

    '-w' option appoint process number like this:
        ./pyget.py -w 5 url

    you can download the unfinished file like this:
        ./pyget.py -w 5 filename
        (the file name suffix is usually the '.pyget')


--------------------------The MIT License---------------------------
Copyright (c) 2011 Qi Wang

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"""

import os, time, sys
import signal
import pycurl
from multiprocessing import Manager, Pool, cpu_count
from optparse import OptionParser
try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO


parser = OptionParser()
parser.add_option("-w", "--worker_count", dest="worker_count",
    type="int", default=cpu_count(), help="downloading process count")


(options, args) = parser.parse_args()
if options.worker_count < 1 or options.worker_count > 20:
    sys.stdout.write("Error: worker_count must in range [1-20]\n")
    sys.stdout.flush()
    sys.exit(0)
    
INFO_FILE_TAIL = ".pyget"

piece = 100000

manager = Manager()
downloaded_point_list = []


def get_curl(url):
    curl = pycurl.Curl()
    curl.setopt(pycurl.URL, url)
    curl.setopt(pycurl.FOLLOWLOCATION, 1)
    curl.setopt(pycurl.MAXREDIRS, 5)
    curl.setopt(pycurl.CONNECTTIMEOUT, 30)
    curl.setopt(pycurl.TIMEOUT, 300)
    curl.setopt(pycurl.NOSIGNAL, 1)
    return curl


def get_file_info(url):
    curl = get_curl(url)
    curl.setopt(pycurl.NOPROGRESS, 1)
    curl.setopt(pycurl.NOBODY, 1)
    curl.perform()
    
    return {
        "url": curl.getinfo(pycurl.EFFECTIVE_URL),
        "file_name": os.path.split(curl.getinfo(pycurl.EFFECTIVE_URL))[1],
        "file_size": int(curl.getinfo(
            pycurl.CONTENT_LENGTH_DOWNLOAD)),
    }


def get_throughput(trans_rate):

    if trans_rate > 1024:
        return "%.1fk/s" % (trans_rate / 1000)

    return "%.1fb/s" % trans_rate


def write_content_to_disk(point, content):
    with open(file_name, "r+b") as fp:
        fp.seek(point[0])
        fp.write(content)
        fp.flush()
    
    with open(file_name + INFO_FILE_TAIL, "a") as fp:
        fp.write(str(point) + "_")
        fp.flush()
    

def remove_info_from_disk():
    os.remove(file_name + INFO_FILE_TAIL)

def show_progbar(throughput):
    point_set_len = len(point_set)
    progress = float(
        point_set_len - len(point_list)) / float(point_set_len) * 100
    
    sys.stdout.write(
        "\r[%.0f%%]:%s\t\t" % (progress, throughput))
    sys.stdout.flush()


def download(point):
    curl = get_curl(args[0])

    buf = StringIO()
    curl.setopt(pycurl.RANGE, "%d-%d" % (point[0], point[1]))
    curl.setopt(pycurl.WRITEFUNCTION, buf.write)
    curl.perform()

    global_info["speed_download"] = curl.getinfo(
        pycurl.SPEED_DOWNLOAD)

    write_content_to_disk(point, buf.getvalue())

    buf.close()

    point_list.remove(point)


signal.signal(signal.SIGINT, lambda n, e: sys.exit(0))



if os.path.exists(args[0]) and args[0].endswith(INFO_FILE_TAIL):
    if not os.path.exists(args[0][:-len(INFO_FILE_TAIL)]):
        sys.stdout.write("bad file!\n")
        sys.stdout.flush()
        sys.exit(0)

    with open(args[0], "r") as fp:
        file_name = args[0][:-6]

        args = eval(fp.readline())

        file_info = get_file_info(args[0])
        file_size = file_info["file_size"]        

        downloaded_point_list = [eval(p) for p in fp.read().split("_") if p]    

else:
    file_info = get_file_info(args[0])
    file_name = file_info["file_name"]
    file_size = file_info["file_size"]
    
    if os.path.exists(file_name):
        tail = str(max(
            map(int,
                filter(lambda x: x.isdigit(), [   
                    s.replace(file_name + ".", "") 
                    for s in list(os.walk("."))[0][2]
                        if s.startswith(file_name + ".")
                ])
            ) or [0]
        ) + 1)
        
        file_name = ".".join([file_name, tail])

    open(file_name, "wb").close()    

    with open(''.join([file_name, INFO_FILE_TAIL]), "w") as fp:
        fp.write(str(args) + "\n")
        fp.flush()

        

point_set = set(map(lambda x:
        (x * piece, (x + 1) * piece),
        xrange(0, file_size / piece)))

point_set.add((file_size - (file_size % piece), file_size))

point_list = manager.list(point_set - set(downloaded_point_list))

global_info = manager.dict({"speed_download": 0})


pool = Pool(processes=options.worker_count)
pool.map_async(download, map(lambda x: (x), point_list))
use_time = 0

 
while True:
    if point_list:
        show_progbar(get_throughput(
            global_info["speed_download"] * options.worker_count))

        time.sleep(1)
        use_time += 1

        continue

    sys.stdout.write("""
        \r[100%%]done!-  time:%ds--%s saved\n""" %
        (use_time, file_name))
    sys.stdout.flush()
        
    remove_info_from_disk()

    sys.exit(0)