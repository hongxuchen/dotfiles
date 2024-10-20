#!/usr/bin/env python

import platform
import shutil
import subprocess
import sys

"""
install external utilities based on
* personal use scenarios
* results of nvim's :checkhealth
"""

tool2pkg = {
    "rg": {"apt": "ripgrep", "dnf": "ripgrep"},
}

sys_tools = ["rg", "cmake", "git", "unzip", "wget", "curl", "luarocks"]

install_sys_cmd_template = {
    "apt": ["sudo", "apt", "install"],
    "dnf": ["sudo", "dnf", "install"],
}


def get_sys_install_bin() -> str | None:
    distro_info = platform.freedesktop_os_release()
    distro_id = distro_info["ID"]
    if distro_id in ["ubuntu", "debian"]:
        return "apt"
    elif distro_id in ["rhel"]:
        return "dnf"
    else:
        return None


def install_sys():
    print("==> sys package installation")
    install_bin = get_sys_install_bin()
    if install_bin is None:
        print("OS distro not supported", file=sys.stderr)
        return
    if not shutil.which(install_bin):
        print(f"sys package {install_bin} not in $PATH")
        return
    for tool in sys_tools:
        fpath = shutil.which(tool)
        if not fpath:
            try:
                pkg = tool2pkg[tool][install_bin]
            except Exception:
                pkg = tool
            cmd = install_sys_cmd_template[install_bin] + [pkg]
            print(f"-> {' '.join(cmd)}")
            rc = subprocess.call(cmd)
            if rc != 0:
                print(f"rc={rc} when running {' '.join(cmd)}", file=sys.stderr)
        else:
            print(f"{tool} found: {fpath}")


go_pkgs = {
    # "duf": "github.com/muesli/duf@latest",
    "errcheck": "github.com/kisielk/errcheck@latest",
    "gomacro": "github.com/cosmos72/gomacro@latest",
    "gorun": "github.com/erning/gorun@latest",
    "lazydocker": "github.com/jesseduffield/lazydocker@latest",
    # "lemonade": "github.com/lemonade-command/lemonade@latest",
    "scc": "github.com/boyter/scc@master",
    # "scip": "github.com/sourcegraph/scip.git@latest",
}


def install_gopkgs():
    print("==> go package installation")
    for k, v in go_pkgs.items():
        fpath = shutil.which(k)
        if fpath:
            print(f"{k} found: {fpath}")
        else:
            cmd = ["go", "install", v]
            print("-> " + " ".join(cmd))
            rc = subprocess.call(cmd)
            if rc != 0:
                print(f"rc={rc} when running {' '.join(cmd)}", file=sys.stderr)


if __name__ == "__main__":
    install_gopkgs()
    install_sys()
