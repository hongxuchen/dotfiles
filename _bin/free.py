#!/usr/bin/env python
import psutil


def main():
    virt = psutil.virtual_memory()
    swap = psutil.swap_memory()
    templ = "{:7s} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}"
    print(templ.format("", "total", "used", "free", "shared", "buffers", "cache"))
    print(
        templ.format(
            "Mem:",
            int(virt.total / 1024),
            int(virt.used / 1024),
            int(virt.free / 1024),
            int(getattr(virt, "shared", 0) / 1024),
            int(getattr(virt, "buffers", 0) / 1024),
            int(getattr(virt, "cached", 0) / 1024),
        ),
    )
    print(
        templ.format(
            "Swap:",
            int(swap.total / 1024),
            int(swap.used / 1024),
            int(swap.free / 1024),
            "",
            "",
            "",
        ),
    )


if __name__ == "__main__":
    main()
