open Yojson

type relay = {
  remote_asn: int32;
  remote_id: Ipaddr.V4.t;
  remote_port: int;
  
  local_asn: int32;
  local_id: Ipaddr.V4.t;
  local_port: int;
}

type config = {
  relays: relay list;
}

let relay_to_string { remote_asn; remote_id; remote_port; local_asn; local_id; local_port } =
  Printf.sprintf "{ remote_asn: %d, remote_id: %s, remote_port: %d, local_asn %d, local_id: %s, local_port: %d }" 
                  (Int32.to_int remote_asn) (Ipaddr.V4.to_string remote_id) remote_port
                  (Int32.to_int local_asn) (Ipaddr.V4.to_string local_id) local_port
;;

let config_to_string { relays } = 
  let str_peers = String.concat "\n" (List.map (fun p -> relay_to_string p) relays) in
  Printf.sprintf " %s" str_peers
;;

let parse json_config = 
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
    let local_asn = 
      Basic.Util.member "local_asn" json_peer 
      |> Basic.Util.to_int 
      |> Int32.of_int 
    in
    let local_id = 
      Basic.Util.member "local_id" json_peer
      |> Basic.Util.to_string
      |> Ipaddr.V4.of_string_exn
    in
    let local_port = 
      Basic.Util.member "local_port" json_peer
      |> Basic.Util.to_int
    in
    { remote_asn; remote_id; remote_port; local_asn; local_id; local_port }
  in
  let json_relays = 
    Basic.Util.member "relays" json_config |> Basic.Util.to_list 
  in

  let relays = List.map f json_relays in

  { relays }
;;

let parse_from_string data = 
  let json_config = Basic.from_string data in
  parse json_config
;;

let parse_from_file file_name = 
  let json_config = Basic.from_file file_name in
  parse json_config
;;

(* let () = 
  let config = parse_from_file "bgpd.json" in
  Printf.printf "%s \n" (config_to_string config)
;; *)



