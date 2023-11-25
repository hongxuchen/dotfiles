#!/usr/bin/env python3

import socket

import psutil

af_map = {
    socket.AF_INET: "IPv4",
    socket.AF_INET6: "IPv6",
    psutil.AF_LINK: "MAC",
}

duplex_map = {
    psutil.NIC_DUPLEX_FULL: "full",
    psutil.NIC_DUPLEX_HALF: "half",
    psutil.NIC_DUPLEX_UNKNOWN: "?",
}


def main():
    stats = psutil.net_if_stats()
    io_counters = psutil.net_io_counters(pernic=True)
    for nic, addrs in list(psutil.net_if_addrs().items()):
        print(f"{nic}:")
        if nic in stats:
            st = stats[nic]
            print("    stats          : ", end="")
            print(
                "speed={}MB, duplex={}, mtu={}, up={}".format(
                    st.speed,
                    duplex_map[st.duplex],
                    st.mtu,
                    "yes" if st.isup else "no",
                ),
            )
        if nic in io_counters:
            io = io_counters[nic]
            print("    incoming       : ", end="")
            print(
                f"bytes={io.bytes_recv}, pkts={io.packets_recv}, errs={io.errin}, drops={io.dropin}",
            )
            print("    outgoing       : ", end="")
            print(
                f"bytes={io.bytes_sent}, pkts={io.packets_sent}, errs={io.errout}, drops={io.dropout}",
            )
        for addr in addrs:
            print("    %-4s" % af_map.get(addr.family, addr.family), end="")
            print(f" address   : {addr.address}")
            if addr.broadcast:
                print(f"         broadcast : {addr.broadcast}")
            if addr.netmask:
                print(f"         netmask   : {addr.netmask}")
            if addr.ptp:
                print(f"      p2p       : {addr.ptp}")
        print("")


if __name__ == "__main__":
    main()
