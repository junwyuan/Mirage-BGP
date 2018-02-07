type t = {
  local_id: Ipaddr.V4.t;
  remote_port: int;
  remote_id: Ipaddr.V4.t;
  local_asn: int32;
}

(* Fixed config *)
let quagga_relay1 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.1";
  remote_port = 50000;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 4_l;
}

let quagga_relay2 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.2";
  remote_port = 50001;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 5_l;
}

let dev_relay1 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.3";
  remote_port = 50002;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 4_l;
}

let dev_relay2 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.4";
  remote_port = 50003;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 5_l;
}

let xen_relay1 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.3";
  remote_port = 179;
  remote_id = Ipaddr.V4.of_string_exn "172.19.0.3";
  local_asn = 4_l;
}

let xen_relay2 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.4";
  remote_port = 179;
  remote_id = Ipaddr.V4.of_string_exn "172.19.0.3";
  local_asn = 5_l;
}

let frr_relay1 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.5";
  remote_port = 50005;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 4_l;
}

let frr_relay2 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.6";
  remote_port = 50006;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 5_l;
}

let xorp_relay1 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.7";
  remote_port = 50007;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 4_l;
}

let xorp_relay2 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.8";
  remote_port = 50008;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 5_l;
}

let bird_relay1 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.9";
  remote_port = 50009;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 4_l;
}

let bird_relay2 = {
  local_id = Ipaddr.V4.of_string_exn "172.19.10.10";
  remote_port = 50010;
  remote_id = Ipaddr.V4.of_string_exn "127.0.0.1";
  local_asn = 5_l;
}
