#!/usr/bin/env python

import os
import shutil
import subprocess
import sys
from pathlib import Path

passwd = "changeit"


def get_java_home() -> str:
    name = "java"
    if "java" not in name:
        print(f"{name} not contains 'java'", file=sys.stderr)
        sys.exit(1)

    abspath = shutil.which(name)
    if abspath is None:
        print(f"cannot find {name}", file=sys.stderr)
        sys.exit(1)

    progpath = Path(abspath)
    if progpath.is_symlink():
        progpath = progpath.resolve()

    parent_path = progpath.parent
    if not parent_path.name == "bin":
        print(f"unexpected parent: {parent_path}", file=sys.stderr)
        sys.exit(1)
    java_home = str(parent_path.parent)

    os.environ["JAVA_HOME"] = java_home

    return java_home


def get_certs(path) -> dict[str, str]:
    certs = {}
    for f in os.listdir(path):
        if not os.path.isfile(f) or not f.endswith(".pem"):
            print(f"unexpected: {f}", file=sys.stderr)
            continue
        abspath = os.path.join(path, f)
        name = os.path.splitext(f)[0]
        certs[abspath] = name
    return certs


def import_certs(java_home: str, certs: dict[str, str]):
    java_cert_dir = os.path.join(java_home, "lib", "security")
    os.chdir(java_cert_dir)
    print(f"WorkingDir: {java_cert_dir}")
    cmd_ss = "keytool -import -trustcacerts -keystore cacerts -storepass {} -noprompt -file {} -alias {}"
    for cert_fpath, name in certs.items():
        cmd_str = cmd_ss.format(passwd, cert_fpath, name)
        print(f"cmd: {cmd_str}")
        subprocess.call(cmd_str.split())


if len(sys.argv) < 2:
    print(f"usage: {sys.argv[0]} PATH")
    sys.exit(1)
certs_dir = sys.argv[1]
certs = get_certs(certs_dir)
if len(certs) == 0:
    print(f"no certs found in {certs_dir}", file=sys.stderr)
    sys.exit(1)
java_home = get_java_home()
import_certs(java_home, certs)
