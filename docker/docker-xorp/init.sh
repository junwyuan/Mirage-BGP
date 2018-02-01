#!/bin/bash

function start {
    ifconfig eth0 172.19.0.7 netmask 255.255.0.0

    sleep 1

    # Start XORP in daemon mode
    /usr/local/xorp/sbin/xorp_rtrmgr -d -P /var/run/xorp.pid -l /var/log/xorp -b /etc/xorp/config.boot

    # Ensure XORP socket permissions are correct (required for XORP 1.7 fixed in 1.8)
    sleep 1
    chmod 775 /var/tmp/xrl.*
    chown root:xorp /var/tmp/xrl.*
}
function stop {
    # Kill the XORP router manager
    killall -q -9 xorp_rtrmgr

    # Kill any lingering XORP sub-processes (updated for 1.8)
    killall -q -9 xorp_bgp
    killall -q -9 xorp_fea
    killall -q -9 xorp_fea_dummy
    killall -q -9 xorp_fib2mrib
    killall -q -9 xorp_igmp
    killall -q -9 xorp_mld
    killall -q -9 xorp_olsr4
    killall -q -9 xorp_ospfv2
    killall -q -9 xorp_ospfv3
    killall -q -9 xorp_pimsm4
    killall -q -9 xorp_pimsm6
    killall -q -9 xorp_policy
    killall -q -9 xorp_rib
    killall -q -9 xorp_rip
    killall -q -9 xorp_ripng
    killall -q -9 xorp_static_routes
    killall -q -9 xorp_vrrp

    # Remove xorp routes from the routing table
    ip route flush proto xorp

    # Clean up XORP sockets
    rm /var/tmp/xrl.*

    # Remove PID file
    rm /var/run/xorp.pid
}
function restart {
    stop
    sleep 1
    start
}

case "$1" in
    start)
        echo -n "Starting XORP: "
        start
        echo "Done"
        ;;
    restart)
        echo -n "XORP restarting... "
        restart
        echo "done"
        ;;
    stop)
        echo -n "Stopping XORP: "
        stop
        echo "Done"
        ;;
    *)
        echo "Usage: $0 {start|stop|restart}"
        exit 1
        ;;
esac
exit 0