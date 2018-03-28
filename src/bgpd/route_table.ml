open Route_injector

type bit = bool

(* Convert a prefix to its bit representation *)
let prefix_to_bits pfx = 
  let subnet = Ipaddr.V4.Prefix.network pfx in
  let l_mask = Ipaddr.V4.Prefix.bits pfx in
  let v = Ipaddr.V4.to_int32 subnet in

  let rec loop_pfx_to_bits acc n =
    if n = 0 then acc
    else 
      let aux = Int32.shift_left 1_l (32 - n) in
      let bit = Int32.((logand v aux) = aux) in
      loop_pfx_to_bits (bit::acc) (n - 1)
  in
  loop_pfx_to_bits [] l_mask
;;

let ip_to_bits ip = 
  let v = Ipaddr.V4.to_int32 ip in

  let rec loop_pfx_to_bits acc n =
    if n = 0 then acc
    else 
      let aux = Int32.shift_left 1_l (32 - n) in
      let bit = Int32.((logand v aux) = aux) in
      loop_pfx_to_bits (bit::acc) (n - 1)
  in
  loop_pfx_to_bits [] 32
;;

let option_get = function
  | None -> 
    Logs.debug (fun m -> m "option_get is called on None.");
    assert false
  | Some v -> v
;;

let rec list_remove v = function 
  | [] -> []
  | hd::tl -> match hd = v with
    | true -> tl
    | false -> hd::(list_remove v tl)
;;

type route = Route_injector.route

let route_to_string route = 
  Printf.sprintf "(net: %s, gw: %s, iface: %s)"
  (Ipaddr.V4.Prefix.to_string route.net)
  (match route.gw with None -> "*" | Some ip -> Ipaddr.V4.to_string ip)
  (match route.iface with None -> "*" | Some s -> s)
;;


module Prefix_map = Map.Make(Ipaddr.V4.Prefix)

(* The separation between prefix matching and information storage is because *)
(* there could be a lot of empty nodes within the trie *)
type t = {
  trie: (bool, Ipaddr.V4.Prefix.t) Prefix_trie.t;
  map: route Prefix_map.t;
}

let print t = 
  let f _ r = 
    Printf.printf "%s\n" (route_to_string r)
  in
  Prefix_map.iter f t.map;
  Printf.printf "-------\n"
;;

let empty = 
  {
    trie = Prefix_trie.empty;
    map = Prefix_map.empty;
  }

(* Gives direct gateway if resolvable *)
let resolve_opt t target_net gw =
  let rec aux_resolve ip = 
    match Prefix_trie.find_opt t.trie (ip_to_bits ip) with
    | None -> 
      (* There is no route matching this ip. *)
      None
    | Some matched_pfx ->
      let route = Prefix_map.find matched_pfx t.map in
      match route.gw with
      | Some next_ip -> begin
        if not (Ipaddr.V4.Prefix.mem next_ip target_net) then 
          match route.iface with
          | None -> aux_resolve next_ip
          | Some _ -> Some (next_ip, route.metric)
        else
          let p = Prefix_trie.find t.trie (ip_to_bits next_ip) in
          if Ipaddr.V4.Prefix.subset target_net p then
            (* Mutual resolved *)
            None
          else 
            match route.iface with
            | None -> aux_resolve next_ip
            | Some _ -> Some (next_ip, route.metric)
      end
      | None -> begin
        match route.iface with
        | Some v -> Some (ip, route.metric)
        | None -> 
          (* Error case: One route has neither iface nor gw. *)
          Logs.err (fun m -> m "One route has neither iface nor gw.");
          assert false
      end
  in
  aux_resolve gw
;;

let resolve t net gw = 
  match resolve_opt t net gw with
  | None -> 
    Logs.err (fun m -> m "Resolve fails.");
    raise Not_found
  | Some v -> v
;;

let resolvable t net gw = resolve_opt t net gw <> None    

let add_static t route = 
  let trie = Prefix_trie.set t.trie (prefix_to_bits route.net) route.net in
  let map = Prefix_map.add route.net route t.map in
  { trie; map }
;;

let cardinal t = Prefix_map.cardinal t.map

let test () = 
  Printexc.record_backtrace true;

  (* Test convertor *)
  let pfx = Ipaddr.V4.(Prefix.make 32 (of_string_exn "255.255.255.255")) in
  let l = prefix_to_bits pfx in
  let s = String.concat "" (List.map (fun b -> if b then "1" else "0") l) in
  assert (s = "11111111111111111111111111111111");

  let pfx = Ipaddr.V4.(Prefix.make 8 (of_string_exn "127.0.0.0")) in
  let l = prefix_to_bits pfx in
  let s = String.concat "" (List.map (fun b -> if b then "1" else "0") l) in
  assert (s = "01111111");

  let pfx = Ipaddr.V4.(Prefix.make 16 (of_string_exn "127.0.0.0")) in
  let l = prefix_to_bits pfx in
  let s = String.concat "" (List.map (fun b -> if b then "1" else "0") l) in
  assert (s = "0111111100000000");


  (* Test prefix tire *)
  let trie = Prefix_trie.create () in

  let pfx1 = Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "10.10.0.0") in
  let trie = Prefix_trie.set trie (prefix_to_bits pfx1) pfx1 in

  let pfx2 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.10.20.0") in
  let trie = Prefix_trie.set trie (prefix_to_bits pfx2) pfx2 in

  let ip1 = Ipaddr.V4.of_string_exn "10.10.0.5" in
  assert (Prefix_trie.find trie (ip_to_bits ip1) = pfx1);

  let ip2 = Ipaddr.V4.of_string_exn "10.10.20.1" in
  assert (Prefix_trie.find trie (ip_to_bits ip2) = pfx2);

  let ip3 = Ipaddr.V4.of_string_exn "20.20.0.1" in
  assert (Prefix_trie.find_opt trie (ip_to_bits ip3) = None);

  let pfx3 = Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "20.20.0.0") in
  let ip4 = Ipaddr.V4.of_string_exn "10.10.20.253" in
  let iface1 = "eth0" in

  (* Test list_remove *)
  let l = [1; 2; 3] in
  assert (list_remove 2 l = [1; 3]);
  assert (list_remove 4 l = [1; 2; 3]);

  (* Test resolve *)
  let t = empty in
  let route1 = {
    metric = 0;
    net = pfx1;
    iface = Some iface1;
    gw = None;
  } in
  let t = add_static t route1 in

  let route2 = {
    metric = 0;
    net = pfx3;
    iface = None;
    gw = Some ip4;
  } in
  let t = add_static t route2 in
  
  (* Resovled *)
  let pfx4 = Ipaddr.V4.(Prefix.make 16 (of_string_exn "40.40.0.0")) in
  let ip5 = Ipaddr.V4.of_string_exn "20.20.0.253" in
  
  match resolve_opt t pfx4 ip5 with
  | None -> assert false
  | Some (gw, m) -> assert (gw = ip4);

  (* Mutual resolved *)
  let pfx5 = Ipaddr.V4.(Prefix.make 24 (of_string_exn "10.10.20.0")) in
  assert (resolve_opt t pfx5 ip5 = None);

  (* No match *)
  let pfx6 = Ipaddr.V4.(Prefix.make 16 (of_string_exn "50.50.0.0")) in
  let ip6 = Ipaddr.V4.of_string_exn "10.0.0.253" in
  assert (resolve_opt t pfx6 ip6 = None);

  (* False mutual resolved *)
  let route3 = {
    net = pfx4;
    gw = Some ip5;
    iface = None;
    metric = 0;
  } in
  let t = add_static t route3 in
  let pfx9 = Ipaddr.V4.(Prefix.make 8 (of_string_exn "20.0.0.0")) in
  let ip9 = Ipaddr.V4.of_string_exn "40.40.0.1" in
  assert (resolvable t pfx9 ip9);
;;


