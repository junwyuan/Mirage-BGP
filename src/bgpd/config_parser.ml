open Yojson
open Filter

type peer = {
  remote_asn: int32;
  remote_id: Ipaddr.V4.t;
  remote_port: int;
  hold_time: int;
  conn_retry_time: int;
  peer_group: int option;
  inbound_filter: route_map option;
}

type config = {
  local_asn: int32;
  local_id: Ipaddr.V4.t;
  local_port: int;
  
  network: Ipaddr.V4.Prefix.t list;
  prefix_lists: prefix_list list;
  route_maps: route_map list;
  
  peers: peer list;
}

let str_to_words str =
  let trimed = String.trim str in
  let words = 
    List.filter (fun x -> x <> "") (String.split_on_char (Char.chr 32) trimed) 
  in
  words
;;

let parse_pfxs json =
  let l = Basic.Util.to_list json in
  let f x = Ipaddr.V4.Prefix.of_string_exn (Basic.Util.to_string x) in
  List.map f l
;;

let parse_entry prefix_lists json =
  let order = Basic.Util.to_int (Basic.Util.member "order" json) in
  let permit = Basic.Util.to_bool (Basic.Util.member "permit" json) in
  
  let parse_condition json = 
    let words = str_to_words (Basic.Util.to_string json) in
    match List.nth words 0 with
    | "prefix_list" ->
      let list_name = List.nth words 1 in
      let f pl = pl.list_name = list_name in
      Prefix_list (List.find f prefix_lists)
    | _ -> raise (Invalid_argument "invalid route map condition")
  in
  let conditions = List.map parse_condition (Basic.Util.to_list (Basic.Util.member "conditions" json)) in

  let parse_action json =
    let words = str_to_words (Basic.Util.to_string json) in
    match List.nth words 0 with
    | "set_weight" -> 
      let weight = int_of_string (List.nth words 1) in
      Set_weight weight
    | _ -> raise (Invalid_argument "invalid route map action")
  in
  let actions = List.map parse_action (Basic.Util.to_list (Basic.Util.member "actions" json)) in

  { order; permit; conditions; actions }
;;

let parse_entries prefix_lists json =
  let tmp = Basic.Util.to_list json in
  List.map (parse_entry prefix_lists) tmp
;;
    

let parse_neighbor route_maps json =
  let remote_asn = 
    Basic.Util.member "remote_asn" json 
    |> Basic.Util.to_int 
    |> Int32.of_int 
  in
  
  let remote_id = 
    Basic.Util.member "remote_id" json
    |> Basic.Util.to_string
    |> Ipaddr.V4.of_string_exn
  in
  
  let remote_port = 
    let tmp = Basic.Util.member "remote_port" json in
    match Basic.Util.to_option (Basic.Util.to_int) tmp with
    | None -> 179
    | Some v -> v
  in

  let hold_time = 
    let tmp = Basic.Util.member "hold_time" json in
    match Basic.Util.to_option (Basic.Util.to_int) tmp with
    | None -> 180
    | Some v -> v
  in

  let conn_retry_time = 
    let tmp = Basic.Util.member "conn_retry_time" json in
    match Basic.Util.to_option (Basic.Util.to_int) tmp with
    | None -> 240
    | Some v -> v
  in

  let inbound_filter =
    let tmp = Basic.Util.member "inbound_filter" json in
    match Basic.Util.to_string_option tmp with
    | None -> None
    | Some str ->
      let words = str_to_words str in
      let p r = r.map_name = (List.nth words 1) in
      let route_map = List.find p route_maps in
      Some route_map
  in  

  let peer_group =
    let tmp = Basic.Util.member "peer_group" json in
    Basic.Util.to_int_option tmp
  in  
  
  { 
    remote_asn; 
    remote_id; 
    remote_port; 
    hold_time; 
    conn_retry_time;
    peer_group;
    inbound_filter;
  }
;;
    
  

let default_config = {
  local_asn = 10_l;
  local_id = Ipaddr.V4.of_string_exn "172.19.0.3";
  local_port = 179;
  
  network = [];
  prefix_lists = [];
  route_maps = [];
  
  peers = [
    {
      remote_asn = 4_l;
      remote_id = Ipaddr.V4.of_string_exn "172.19.10.3";
      remote_port = 179;
      hold_time = 90;
      conn_retry_time = 0;
      inbound_filter = None;
      peer_group = None;
    };
    {
      remote_asn = 5_l;
      remote_id = Ipaddr.V4.of_string_exn "172.19.10.4";
      remote_port = 179;
      hold_time = 90;
      conn_retry_time = 0;
      peer_group = None;
      inbound_filter = None;
    };
  ]
}

let peer_to_string peer =
  Printf.sprintf "{ remote_asn: %d, remote_id: %s, remote_port: %d, hold_time %d, conn_retry_time %d, inbound_filter: %s }" 
            (Int32.to_int peer.remote_asn) 
            (Ipaddr.V4.to_string peer.remote_id) 
            peer.remote_port 
            peer.hold_time
            peer.conn_retry_time
            (match peer.inbound_filter with None -> "None" | Some v -> v.map_name)
;;

let config_to_string config = 
  let str_pl = String.concat "\n" (List.map (fun p -> prefix_list_to_string p) config.prefix_lists) in
  let str_rm = String.concat "\n" (List.map (fun r -> route_map_to_string r) config.route_maps) in
  let str_n = Printf.sprintf "Network: [%s]" (String.concat ";" (List.map (fun x -> Ipaddr.V4.Prefix.to_string x) config.network)) in
  let str_peers = String.concat "\n" (List.map (fun p -> peer_to_string p) config.peers) in

  Printf.sprintf " local_asn: %d, local_id %s, local_port: %d\n%s\nPrefix_list\n%s\nRoute-map\n%s\nPeers\n%s" 
                  (Int32.to_int config.local_asn)
                  (Ipaddr.V4.to_string config.local_id) 
                  config.local_port 
                  str_n str_pl str_rm str_peers
;;

let parse json = 
  let local_asn = 
    Basic.Util.member "local_asn" json 
    |> Basic.Util.to_int 
    |> Int32.of_int 
  in

  let local_id = 
    Basic.Util.member "local_id" json
    |> Basic.Util.to_string
    |> Ipaddr.V4.of_string_exn
  in

  let local_port = 
    let tmp = Basic.Util.member "local_port" json in
    match Basic.Util.to_option (Basic.Util.to_int) tmp with
    | None -> 179
    | Some v -> v
  in

  let network = 
    let tmp = Basic.Util.member "network" json in
    match Basic.Util.to_option parse_pfxs tmp with
    | None -> []
    | Some v -> v
  in

  let key_and_values = Basic.Util.to_assoc json in

  let prefix_lists =
    let f acc (k, v) = 
      let words = str_to_words k in
      match List.nth words 0 with
      | "prefix_list" -> 
        let list_name = List.nth words 1 in
        let entries = parse_pfxs v in
        { list_name; entries; }::acc
      | _ -> acc
    in
    List.fold_left f [] key_and_values
  in

  let route_maps =
    let f acc (k, v) = 
      let words = str_to_words k in
      match List.nth words 0 with
      | "route_map" -> 
        let map_name = List.nth words 1 in
        let entries = parse_entries prefix_lists v in
        { map_name; entries; }::acc
      | _ -> acc
    in
    List.fold_left f [] key_and_values
  in

  let peers = 
    let f acc (k, v) = 
      let words = str_to_words k in
      match List.nth words 0 with
      | "neighbor" -> 
        let neighbor = parse_neighbor route_maps v in
        neighbor::acc
      | _ -> acc
    in
    List.fold_left f [] key_and_values
  in

  { 
    local_asn; 
    local_id; 
    local_port; 

    network;
    prefix_lists;
    route_maps;

    peers; 
  }
;;

let parse_from_string data = 
  let json_config = Basic.from_string data in
  parse json_config
;;

let parse_from_file file_name = 
  let json_config = Basic.from_file file_name in
  parse json_config
;;

let () = 
  Printexc.record_backtrace true;
  let config = parse_from_file "config/bgpd.json" in
  Printf.printf "%s \n" (config_to_string config)
;;
