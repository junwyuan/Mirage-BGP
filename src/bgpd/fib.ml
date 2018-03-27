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


(* Use mutable field to reduce the need for garbage collection *)
type route = {
  net: Ipaddr.V4.Prefix.t;
  inter_gw: Ipaddr.V4.t option;
  iface: string option;
  direct_gw: Ipaddr.V4.t option;
  mutable dependents: Ipaddr.V4.Prefix.t list;
}

let route_to_string route = 
  Printf.sprintf "(net: %s, gw: %s, iface: %s, dependents: %s)"
  (Ipaddr.V4.Prefix.to_string route.net)
  (match route.inter_gw with None -> "*" | Some ip -> Ipaddr.V4.to_string ip)
  (match route.iface with None -> "*" | Some s -> s)
  (String.concat "; " (List.map (fun p -> Ipaddr.V4.Prefix.to_string p) route.dependents))
;;


module Prefix_map = Map.Make(Ipaddr.V4.Prefix)

(* The separation between prefix matching and information storage is because *)
(* there could be a lot of empty nodes within the trie *)
type t = {
  trie: (bool, Ipaddr.V4.Prefix.t) Prefix_trie.t;
  map: route Prefix_map.t;
}

let print t = 
  Prefix_map.iter (fun _ route -> Printf.printf "%s\n" (route_to_string route)) t.map

let empty = 
  {
    trie = Prefix_trie.empty;
    map = Prefix_map.empty;
  }


let resolve_opt t net inter_gw =
  let rec aux_resolve ip used = 
    match Prefix_trie.find_opt t.trie (ip_to_bits ip) with
    | None -> 
      (* There is no route matching this ip. *)
      None
    | Some pfx ->
      let route = Prefix_map.find pfx t.map in
      match route.inter_gw with
      | Some next_ip -> begin
        if Ipaddr.V4.Prefix.mem next_ip net then 
          (* Mutual resolved. *)
          None
        else 
          match route.iface with
          | None -> aux_resolve next_ip (pfx::used)
          | Some v -> Some (next_ip, v, (pfx::used))
      end
      | None -> begin
        match route.iface with
        | Some v -> Some (ip, v, (pfx::used))
        | None -> 
          (* Error case: One route has neither iface nor gw. *)
          Logs.err (fun m -> m "One route has neither iface nor gw.");
          assert false
      end
  in
  aux_resolve inter_gw []
;;

let resolve t net inter_gw = 
  match resolve_opt t net inter_gw with
  | None -> 
    Logs.err (fun m -> m "Resolve fails.");
    raise Not_found
  | Some v -> v
;;

let resolvable t net inter_gw = resolve_opt t net inter_gw <> None    

(* Warning: would not add the route if it is not resolvable. *)
let add t net inter_gw =
  match resolve_opt t net inter_gw with
  | None -> 
    (* This route is not resolvable. *)
    t
  | Some (direct_gw, iface, new_used) -> 
    match Prefix_map.find_opt net t.map with
    | Some old_route ->
      (* This prefix exists already, this is a replace operation. *)
      
      (* Update what this route depends on. This part is impure. *)
      let () = 
        let _, _, old_used = resolve t net (option_get old_route.inter_gw) in 

        let rm_old_dep pfx = 
          match Prefix_map.find_opt pfx t.map with
          | None ->
            Logs.err (fun m -> m "A route in trie does not have a corresponding record in map.");
            assert false
          | Some route ->
            let f acc pfx = list_remove pfx acc in
            route.dependents <- List.fold_left f route.dependents (net::old_route.dependents)
        in
        let () = List.iter rm_old_dep old_used in

        let add_new_dep pfx = 
          match Prefix_map.find_opt pfx t.map with
          | None ->
            Logs.err (fun m -> m "A route in trie does not have a corresponding record in map.");
            assert false
          | Some route ->
            route.dependents <- (net::old_route.dependents) @ route.dependents
        in
        List.iter add_new_dep new_used
      in

      let new_route = { 
        net; 
        inter_gw = Some inter_gw;
        direct_gw = Some direct_gw; 
        iface = None;
        dependents = old_route.dependents;
      } in
      let new_map = Prefix_map.add net new_route t.map in

      { trie = t.trie; map = new_map }
    | None ->
      (* This prefix does not exist before, this is an insertion. *)
      let tmp = Ipaddr.V4.Prefix.network net in
      match Prefix_trie.find_opt t.trie (ip_to_bits tmp) with
      | None ->
        (* This route brand new route. *)
        let route = { 
          net; 
          inter_gw = Some inter_gw;
          direct_gw = Some direct_gw; 
          iface = None;
          dependents = [];
        } in
        let new_map = Prefix_map.add net route t.map in

        let add_new_dep pfx = 
          match Prefix_map.find_opt pfx new_map with
          | None ->
            Logs.err (fun m -> m "A route in trie does not have a corresponding record in map.");
            assert false
          | Some route ->
            route.dependents <- net::route.dependents
        in
        let () = List.iter add_new_dep new_used in

        let new_trie = Prefix_trie.set t.trie (prefix_to_bits net) net in

        { trie = new_trie; map = new_map; }
      | Some sup_net ->
        (* This 'net' is a subnet of some existing net. *)
        let sup_route = Prefix_map.find sup_net t.map in

        let route = { 
          net; 
          inter_gw = Some inter_gw;
          direct_gw = Some direct_gw; 
          iface = None;
          dependents = [];
        } in
        let new_map = Prefix_map.add net route t.map in

        let add_new_dep pfx = 
          match Prefix_map.find_opt pfx new_map with
          | None ->
            Logs.err (fun m -> m "A route in trie does not have a corresponding record in map.");
            assert false
          | Some route ->
            route.dependents <- net::route.dependents
        in
        let () = List.iter add_new_dep new_used in

        let new_trie = Prefix_trie.set t.trie (prefix_to_bits net) net in
        
        let new_t = { trie = new_trie; map = new_map; } in

        let depend pfx =  
          let r = Prefix_map.find pfx new_t.map in
          if Ipaddr.V4.Prefix.mem (option_get r.inter_gw) net then true
          else
            let _, _, used = resolve new_t pfx (option_get r.inter_gw) in
            let p pfx =
              let r = Prefix_map.find pfx new_t.map in
              match r.inter_gw with
              | None -> false
              | Some ip -> Ipaddr.V4.Prefix.mem ip net
            in
            List.exists p used
        in

        (* l1 is the set of prefixes that depend on the new route *)
        (* l2 is the set of prefixes that depend on the old route *)
        let l1, l2 = List.partition depend sup_route.dependents in
        let () = sup_route.dependents <- l2 in
        let () = route.dependents <- l1 in
        
        new_t
;;

let remove t net = 
  match Prefix_map.find_opt net t.map with
  | None -> 
    (* The prefix does not exist yet *)
    t, []
  | Some route ->     
    let new_map = Prefix_map.remove net t.map in
    let new_trie = Prefix_trie.unset t.trie (prefix_to_bits net) in
    
    (* remove deps *)
    let _, _, used = resolve t net (option_get (route.inter_gw)) in
    let rm_old_dep pfx = 
      match Prefix_map.find_opt pfx new_map with
      | None ->
        Logs.err (fun m -> m "A route in trie does not have a corresponding record in map.");
        assert false
      | Some route ->
        let f acc pfx = list_remove pfx acc in
        route.dependents <- List.fold_left f route.dependents (net::route.dependents)
    in
    let () = List.iter rm_old_dep used in
    let new_t = { trie = new_trie; map = new_map; } in

    (* find those that are invalid after this route is removed *)
    let f (t, wd) pfx = 
      let route = Prefix_map.find pfx t.map in
      let new_map = Prefix_map.remove pfx t.map in
      let new_trie = Prefix_trie.unset t.trie (prefix_to_bits pfx) in
      let new_t = { trie = new_trie; map = new_map; } in
      match route.inter_gw with
      | None -> (t, wd)
      | Some gw -> begin
        match resolve_opt new_t pfx gw with
        | None -> (new_t, pfx::wd)
        | Some _ -> (add new_t pfx gw, wd)
      end
    in
    List.fold_left f (new_t, [ net ]) route.dependents
;;


let add_static t net inter_gw iface = 
  let route = { net; inter_gw; iface; direct_gw = None; dependents = []; } in
  let trie = Prefix_trie.set t.trie (prefix_to_bits net) net in
  let map = Prefix_map.add net route t.map in
  { trie; map }
;;

let cardinal t = Prefix_map.cardinal t.map

let () = 
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
  let t = add_static t pfx1 None (Some iface1) in
  let t = add_static t pfx3 (Some ip4) None in
  
  (* Resovled *)
  let pfx4 = Ipaddr.V4.(Prefix.make 16 (of_string_exn "40.40.0.0")) in
  let ip5 = Ipaddr.V4.of_string_exn "20.20.0.253" in
  
  match resolve_opt t pfx4 ip5 with
  | None -> assert false
  | Some (gw, iface, used) ->
    assert (gw = ip4);
    assert (iface = iface1);
    assert (used = [pfx1; pfx3]);

  (* Mutual resolved *)
  let pfx5 = Ipaddr.V4.(Prefix.make 24 (of_string_exn "10.10.20.0")) in
  assert (resolve_opt t pfx5 ip5 = None);

  (* No match *)
  let pfx6 = Ipaddr.V4.(Prefix.make 16 (of_string_exn "50.50.0.0")) in
  let ip6 = Ipaddr.V4.of_string_exn "10.0.0.253" in
  assert (resolve_opt t pfx6 ip6 = None);

  (* Test add/replace/remove *)
  let t = empty in
  let t = add_static t pfx1 None (Some iface1) in

  (* normal insert *)
  let t = add t pfx3 ip1 in
  assert (cardinal t = 2);
  assert (Prefix_map.mem pfx3 t.map);
  assert (Prefix_trie.mem t.trie (prefix_to_bits pfx3));
  let route = Prefix_map.find pfx3 t.map in
  assert (route.net = pfx3);
  assert (route.inter_gw = Some ip1);
  assert (route.iface = None);
  assert (route.direct_gw = Some ip1);
  assert (route.dependents = []);
  let route = Prefix_map.find pfx1 t.map in
  assert (route.dependents = [ pfx3 ]);

  let t = add t pfx4 ip3 in
  let ip7 = Ipaddr.V4.of_string_exn "20.20.1.1" in
  let t = add t pfx6 ip7 in
  let route = Prefix_map.find pfx3 t.map in
  assert (List.length route.dependents = 2);
  assert (List.mem pfx4 route.dependents);
  assert (List.mem pfx6 route.dependents);
  let route = Prefix_map.find pfx1 t.map in
  assert (List.length route.dependents = 3);
  assert (List.mem pfx4 route.dependents);
  assert (List.mem pfx6 route.dependents);
  
  (* insert a sub route *)
  let pfx7 = Ipaddr.V4.(Prefix.make 24 (of_string_exn "20.20.1.0")) in  
  let t = add t pfx7 ip1 in
  assert (cardinal t = 5);
  assert (Prefix_map.mem pfx7 t.map);
  assert (Prefix_trie.mem t.trie (prefix_to_bits pfx7));
  let route = Prefix_map.find pfx3 t.map in
  assert (List.length route.dependents = 1);
  assert (List.mem pfx4 route.dependents);
  let route = Prefix_map.find pfx7 t.map in
  assert (List.length route.dependents = 1);
  assert (List.mem pfx6 route.dependents);
  let route = Prefix_map.find pfx1 t.map in
  assert (List.length route.dependents = 4);



  (* Replace a non-depended route *)
  let t = add t pfx4 ip7 in
  let route = Prefix_map.find pfx4 t.map in
  assert (route.inter_gw = Some ip7);
  assert (route.dependents = []);
  let route = Prefix_map.find pfx3 t.map in
  assert (List.length route.dependents = 0);
  let route = Prefix_map.find pfx7 t.map in
  assert (List.length route.dependents = 2);
  assert (List.mem pfx4 route.dependents);
  assert (List.mem pfx6 route.dependents);
  let route = Prefix_map.find pfx1 t.map in
  assert (List.length route.dependents = 4);

  (* Replace a depended route *)
  let pfx8 = Ipaddr.V4.(Prefix.make 24 (of_string_exn "60.60.0.0")) in  
  let ip8 = Ipaddr.V4.of_string_exn "60.60.0.1" in
  let iface2 = "eth1" in
  let t = add_static t pfx8 None (Some iface2) in
  let t = add t pfx7 ip8 in
  assert (cardinal t = 6);
  let route = Prefix_map.find pfx7 t.map in
  assert (route.inter_gw = Some ip8);
  assert (List.length route.dependents = 2);
  assert (List.mem pfx4 route.dependents);
  assert (List.mem pfx6 route.dependents);
  let route = Prefix_map.find pfx1 t.map in
  assert (List.length route.dependents = 1);
  assert (List.mem pfx3 route.dependents);
  let route = Prefix_map.find pfx8 t.map in
  assert (List.length route.dependents = 3);
  assert (List.mem pfx4 route.dependents);
  assert (List.mem pfx6 route.dependents);
  assert (List.mem pfx7 route.dependents);

  (* Withdraw a non-depended route *)
  let t, wd = remove t pfx3 in
  assert (wd = [ pfx3 ]);
  assert (cardinal t = 5);
  let route = Prefix_map.find pfx1 t.map in
  assert (route.dependents = []);
  assert (not (Prefix_map.mem pfx3 t.map));
  assert (not (Prefix_trie.mem t.trie (prefix_to_bits pfx3)));

  (* Withdraw a depended route *)
  let t, wd = remove t pfx7 in
  assert (List.length wd = 3);
  assert (List.mem pfx4 wd);
  assert (List.mem pfx6 wd);
  assert (List.mem pfx7 wd);
  assert (cardinal t = 2);
  assert (not (Prefix_map.mem pfx7 t.map));
  assert (not (Prefix_trie.mem t.trie (prefix_to_bits pfx7)));
  let route = Prefix_map.find pfx8 t.map in
  assert (route.dependents = []);
;;