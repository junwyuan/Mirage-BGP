open Yojson

type peer = {
  remote_asn: int32;
  remote_id: Ipaddr.V4.t;
  remote_port: int;
  hold_time: int;
  conn_retry_time: int;
}

type config = {
  local_asn: int32;
  local_id: Ipaddr.V4.t;
  local_port: int;
  peers: peer list;
}

let default_config = {
  local_asn = 10_l;
  local_id = Ipaddr.V4.of_string_exn "172.19.0.3";
  local_port = 179;
  peers = [
    {
      remote_asn = 4_l;
      remote_id = Ipaddr.V4.of_string_exn "172.19.10.3";
      remote_port = 179;
      hold_time = 90;
      conn_retry_time = 0;
    };
    {
      remote_asn = 5_l;
      remote_id = Ipaddr.V4.of_string_exn "172.19.10.4";
      remote_port = 179;
      hold_time = 90;
      conn_retry_time = 0;
    };
  ]
}


let peer_to_string { remote_asn; remote_id; remote_port; hold_time } =
  Printf.sprintf "{ remote_asn: %d, remote_id: %s, remote_port: %d, hold_time %d }" (Int32.to_int remote_asn) 
                 (Ipaddr.V4.to_string remote_id) remote_port hold_time
;;

let config_to_string { local_asn; local_id; local_port; peers } = 
  let str_peers = String.concat "\n" (List.map (fun p -> peer_to_string p) peers) in
  Printf.sprintf " local_asn: %d, local_id %s, local_port: %d \n%s" (Int32.to_int local_asn)
                 (Ipaddr.V4.to_string local_id) local_port str_peers
;;

let parse_from_string data = 
  let json_config = Basic.from_string data in
  
  let local_asn = 
    Basic.Util.member "local_asn" json_config 
    |> Basic.Util.to_int 
    |> Int32.of_int 
  in

  let local_id = 
    Basic.Util.member "local_id" json_config
    |> Basic.Util.to_string
    |> Ipaddr.V4.of_string_exn
  in

  let local_port = 
    Basic.Util.member "local_port" json_config
    |> Basic.Util.to_int
  in

  let f json_peer =
    let remote_asn = 
      Basic.Util.member "remote_asn" json_peer 
      |> Basic.Util.to_int 
      |> Int32.of_int 
    in
    let remote_id = 
      Basic.Util.member "remote_id" json_peer
      |> Basic.Util.to_string
      |> Ipaddr.V4.of_string_exn
    in
    let remote_port = 
      Basic.Util.member "remote_port" json_peer
      |> Basic.Util.to_int
    in
    let hold_time = 
      Basic.Util.member "hold_time" json_peer
      |> Basic.Util.to_int
    in
    let conn_retry_time = 
      Basic.Util.member "conn_retry_time" json_peer
      |> Basic.Util.to_int
    in
    { remote_asn; remote_id; remote_port; hold_time; conn_retry_time }
  in
  let json_peers = 
    Basic.Util.member "peers" json_config 
    |> Basic.Util.to_list 
  in

  Printf.printf "%d" (List.length json_peers);

  let peers = List.map f json_peers in

  { local_asn; local_id; local_port; peers }
;;


(* let () = 
  let config = parse_from_file "bgpd.json" in
  Printf.printf "%s \n" (config_to_string config)
;; *)



