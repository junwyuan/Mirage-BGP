protocols {
    bgp {
        bgp-id: 172.19.0.7
        local-as: "65002"
        enable-4byte-as-numbers: false
        peer "172.19.10.7" {
            peer-port: 179
            local-port: 179
            local-ip: "172.19.0.7"
            local-dev: ""
            as: "4"
            next-hop: 172.19.0.7
            holdtime: 90
            delay-open-time: 0
            client: false
            confederation-member: false
            prefix-limit {
                maximum: 250000
                disable: true
            }
            disable: false
            ipv4-unicast: true
            ipv4-multicast: false
            ipv6-unicast: false
            ipv6-multicast: false
            import: ""
            export: ""
        }
        peer "172.19.10.8" {
            peer-port: 179
            local-port: 179
            local-ip: "172.19.0.7"
            local-dev: ""
            as: "5"
            next-hop: 172.19.0.7
            holdtime: 90
            delay-open-time: 0
            client: false
            confederation-member: false
            prefix-limit {
                maximum: 250000
                disable: true
            }
            disable: false
            ipv4-unicast: true
            ipv4-multicast: false
            ipv6-unicast: false
            ipv6-multicast: false
            import: ""
            export: ""
        }
        import: ""
        export: ""
    }
}

fea {
  unicast-forwarding4 {
    disable: true
    forwarding-entries {
        retain-on-startup: true
        retain-on-shutdown: true
    }
  }
}

interfaces {
  restore-original-config-on-shutdown: true
  interface eth0 {
    description: ""
    disable: false
    discard: false
    unreachable: false
    management: false
    parent-ifname: ""
    iface-type: ""
    vid: ""
    default-system-config {
    }
  }
}

rtrmgr {
  config-directory: "/etc/xorp/"
  load-file-command: "fetch"
  load-file-command-args: "-o"
  load-ftp-command: "fetch"
  load-ftp-command-args: "-o"
  load-http-command: "fetch"
  load-http-command-args: "-o"
  load-tftp-command: "sh -c 'echo Not implemented 1>&2 && exit 1'"
  load-tftp-command-args: ""
  save-file-command: "sh -c 'echo Not implemented 1>&2 && exit 1'"
  save-file-command-args: ""
  save-ftp-command: "sh -c 'echo Not implemented 1>&2 && exit 1'"
  save-ftp-command-args: ""
  save-http-command: "sh -c 'echo Not implemented 1>&2 && exit 1'"
  save-http-command-args: ""
  save-tftp-command: "sh -c 'echo Not implemented 1>&2 && exit 1'"
  save-tftp-command-args: ""
}