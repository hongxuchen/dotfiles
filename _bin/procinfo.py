#!/usr/bin/env python

import argparse
import datetime
import socket
import sys

import psutil

ACCESS_DENIED = ""
NON_VERBOSE_ITERATIONS = 4
RLIMITS_MAP = {
    "RLIMIT_AS": "virtualmem",
    "RLIMIT_CORE": "coredumpsize",
    "RLIMIT_CPU": "cputime",
    "RLIMIT_DATA": "datasize",
    "RLIMIT_FSIZE": "filesize",
    "RLIMIT_LOCKS": "locks",
    "RLIMIT_MEMLOCK": "memlock",
    "RLIMIT_MSGQUEUE": "msgqueue",
    "RLIMIT_NICE": "nice",
    "RLIMIT_NOFILE": "openfiles",
    "RLIMIT_NPROC": "maxprocesses",
    "RLIMIT_RSS": "rss",
    "RLIMIT_RTPRIO": "realtimeprio",
    "RLIMIT_RTTIME": "rtimesched",
    "RLIMIT_SIGPENDING": "sigspending",
    "RLIMIT_STACK": "stack",
}


def convert_bytes(n):
    symbols = ("K", "M", "G", "T", "P", "E", "Z", "Y")
    prefix = {}
    for i, s in enumerate(symbols):
        prefix[s] = 1 << (i + 1) * 10
    for s in reversed(symbols):
        if n >= prefix[s]:
            value = float(n) / prefix[s]
            return f"{value:.1f}{s}"
    return f"{n}B"


def pdump(a, b):
    if sys.stdout.isatty() and psutil.POSIX:
        fmt = "\x1b[1;32m%-13s\x1b[0m %s" % (a, b)
    else:
        fmt = "%-11s %s" % (a, b)
    print(fmt)


def str_ntuple(nt, bytes2human=False):
    if nt == ACCESS_DENIED:
        return ""
    if not bytes2human:
        return ", ".join([f"{x}={getattr(nt, x)}" for x in nt._fields])
    else:
        return ", ".join([f"{x}={convert_bytes(getattr(nt, x))}" for x in nt._fields])


def run(pid, verbose=False):
    try:
        proc = psutil.Process(pid)
        pinfo = proc.as_dict(ad_value=ACCESS_DENIED)
    except psutil.NoSuchProcess as err:
        sys.exit(str(err))

    # collect other proc info
    with proc.oneshot():
        try:
            parent = proc.parent()
            if parent:
                parent = f"({parent.name()})"
            else:
                parent = ""
        except psutil.Error:
            parent = ""
        try:
            pinfo["children"] = proc.children()
        except psutil.Error:
            pinfo["children"] = []
        if pinfo["create_time"]:
            started = datetime.datetime.fromtimestamp(pinfo["create_time"]).strftime(
                "%Y-%m-%d %H:%M",
            )
        else:
            started = ACCESS_DENIED

    # here we go
    pdump("pid", pinfo["pid"])
    pdump("name", pinfo["name"])
    pdump("parent", f"{pinfo['ppid']} {parent}")
    pdump("exe", pinfo["exe"])
    pdump("cwd", pinfo["cwd"])
    pdump("cmdline", " ".join(pinfo["cmdline"]))
    pdump("started", started)

    cpu_tot_time = datetime.timedelta(seconds=sum(pinfo["cpu_times"]))
    cpu_tot_time = "{}:{}.{}".format(
        cpu_tot_time.seconds // 60 % 60,
        str(cpu_tot_time.seconds % 60).zfill(2),
        str(cpu_tot_time.microseconds)[:2],
    )
    pdump("cpu-tspent", cpu_tot_time)
    pdump("cpu-times", str_ntuple(pinfo["cpu_times"]))
    if hasattr(proc, "cpu_affinity"):
        pdump("cpu-affinity", pinfo["cpu_affinity"])
    if hasattr(proc, "cpu_num"):
        pdump("cpu-num", pinfo["cpu_num"])

    pdump("memory", str_ntuple(pinfo["memory_info"], bytes2human=True))
    pdump("memory %", round(pinfo["memory_percent"], 2))
    pdump("user", pinfo["username"])
    if psutil.POSIX:
        pdump("uids", str_ntuple(pinfo["uids"]))
    if psutil.POSIX:
        pdump("uids", str_ntuple(pinfo["uids"]))
    if psutil.POSIX:
        pdump("terminal", pinfo["terminal"] or "")

    pdump("status", pinfo["status"])
    pdump("nice", pinfo["nice"])
    if hasattr(proc, "ionice"):
        try:
            ionice = proc.ionice()
        except psutil.Error:
            pass
        else:
            if psutil.WINDOWS:
                pdump("ionice", ionice)
            else:
                pdump("ionice", f"class={str(ionice.ioclass)}, value={ionice.value}")

    pdump("num-threads", pinfo["num_threads"])
    if psutil.POSIX:
        pdump("num-fds", pinfo["num_fds"])
    if psutil.WINDOWS:
        pdump("num-handles", pinfo["num_handles"])

    if "io_counters" in pinfo:
        pdump("I/O", str_ntuple(pinfo["io_counters"], bytes2human=True))
    pdump("ctx-switches", str_ntuple(pinfo["num_ctx_switches"]))
    if pinfo["children"]:
        template = "%-6s %s"
        pdump("children", template % ("PID", "NAME"))
        for child in pinfo["children"]:
            try:
                pdump("", template % (child.pid, child.name()))
            except psutil.AccessDenied:
                pdump("", template % (child.pid, ""))
            except psutil.NoSuchProcess:
                pass

    if pinfo["open_files"]:
        pdump("open-files", "PATH")
        for i, file in enumerate(pinfo["open_files"]):
            if not verbose and i >= NON_VERBOSE_ITERATIONS:
                pdump("", "[...]")
                break
            pdump("", file.path)
    else:
        pdump("open-files", "")

    if pinfo["connections"]:
        template = "%-5s %-25s %-25s %s"
        pdump(
            "connections",
            template % ("PROTO", "LOCAL ADDR", "REMOTE ADDR", "STATUS"),
        )
        for conn in pinfo["connections"]:
            if conn.type == socket.SOCK_STREAM:
                type = "TCP"
            elif conn.type == socket.SOCK_DGRAM:
                type = "UDP"
            else:
                type = "UNIX"
            lip, lport = conn.laddr
            if not conn.raddr:
                rip, rport = "*", "*"
            else:
                rip, rport = conn.raddr
            pdump(
                "",
                template % (type, f"{lip}:{lport}", f"{rip}:{rport}", conn.status),
            )
    else:
        pdump("connections", "")

    if pinfo["threads"] and len(pinfo["threads"]) > 1:
        template = "%-5s %12s %12s"
        pdump("threads", template % ("TID", "USER", "SYSTEM"))
        for i, thread in enumerate(pinfo["threads"]):
            if not verbose and i >= NON_VERBOSE_ITERATIONS:
                pdump("", "[...]")
                break
            pdump("", template % thread)
        pdump("", f"total={len(pinfo['threads'])}")
    else:
        pdump("threads", "")

    if hasattr(proc, "rlimit"):
        res_names = [x for x in dir(psutil) if x.startswith("RLIMIT")]
        resources = []
        for res_name in res_names:
            try:
                soft, hard = proc.rlimit(getattr(psutil, res_name))
            except psutil.AccessDenied:
                pass
            else:
                resources.append((res_name, soft, hard))
        if resources:
            template = "%-12s %15s %15s"
            pdump("res-limits", template % ("RLIMIT", "SOFT", "HARD"))
            for res_name, soft, hard in resources:
                if soft == psutil.RLIM_INFINITY:
                    soft = "infinity"
                if hard == psutil.RLIM_INFINITY:
                    hard = "infinity"
                pdump("", template % (RLIMITS_MAP.get(res_name, res_name), soft, hard))

    if hasattr(proc, "environ") and pinfo["environ"]:
        template = "%-25s %s"
        pdump("environ", template % ("NAME", "VALUE"))
        for i, k in enumerate(sorted(pinfo["environ"])):
            if not verbose and i >= NON_VERBOSE_ITERATIONS:
                pdump("", "[...]")
                break
            pdump("", template % (k, pinfo["environ"][k]))

    if pinfo.get("memory_maps", None):
        template = "%-8s %s"
        pdump("mem-maps", template % ("RSS", "PATH"))
        maps = sorted(pinfo["memory_maps"], key=lambda x: x.rss, reverse=True)
        for i, region in enumerate(maps):
            if not verbose and i >= NON_VERBOSE_ITERATIONS:
                pdump("", "[...]")
                break
            pdump("", template % (convert_bytes(region.rss), region.path))


def main():
    parser = argparse.ArgumentParser(description="print information about a process")
    parser.add_argument("pids", type=int, nargs="+", help="process pid(s)")
    parser.add_argument("--verbose", "-v", action="store_true", help="print more info")
    args = parser.parse_args()
    for pid in args.pids:
        print(f"\n{'='*80}")
        run(pid, args.verbose)


if __name__ == "__main__":
    main()
