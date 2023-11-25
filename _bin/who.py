#!/usr/bin/env python3

from datetime import datetime

import psutil


def main() -> None:
    users = psutil.users()
    for user in users:
        pid = int(user.pid)
        proc_name = psutil.Process(pid).name() if user.pid else ""
        print(
            "%-12s %-10s %-10s %-14s %s"
            % (
                user.name,
                user.terminal or "-",
                datetime.fromtimestamp(user.started).strftime("%Y-%m-%d %H:%M"),
                "(%s)" % user.host if user.host else "",
                proc_name,
            ),
        )


if __name__ == "__main__":
    main()
