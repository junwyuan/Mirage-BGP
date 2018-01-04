type t = {
  id: Ipaddr.V4.t;
  port: int;
  as_no: int32;
}

(* Fixed config *)
let quagga_relay1 = {
  id = Ipaddr.V4.of_string_exn "172.19.10.1";
  port = 50000;
  as_no = 4_l;
}

let quagga_relay2 = {
  id = Ipaddr.V4.of_string_exn "172.19.10.2";
  port = 50001;
  as_no = 5_l;
}

let dev_relay1 = {
  id = Ipaddr.V4.of_string_exn "172.19.10.3";
  port = 50002;
  as_no = 4_l;
}

let dev_relay2 = {
  id = Ipaddr.V4.of_string_exn "172.19.10.4";
  port = 50003;
  as_no = 5_l;
}