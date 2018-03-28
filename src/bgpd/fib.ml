open Tree

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
  static: bool;
  net: Ipaddr.V4.Prefix.t;
  gw: Ipaddr.V4.t option;
  iface: string option;
}

type krt_change = {
  subnet: Ipaddr.V4.Prefix.t;
  direct_gw: Ipaddr.V4.t;
}

type krt_update = {
  add: krt_change list;
  remove: krt_change list;
}

let depended node = Tree.ancestors node

let dependents node = Tree.offspring node

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
  map: route Tree.t Prefix_map.t;
}

let print t = 
  let f _ node = 
    Printf.printf "%s | dependents: %s | depended %s \n" 
                    (route_to_string (Tree.value node))
                    (String.concat "; " (List.map (fun r -> Ipaddr.V4.Prefix.to_string r.net) (dependents node)))
                    (String.concat "; " (List.map (fun r -> Ipaddr.V4.Prefix.to_string r.net) (depended node)))
  in
  Prefix_map.iter f t.map;
  Printf.printf "-------\n"
;;

let empty = 
  {
    trie = Prefix_trie.empty;
    map = Prefix_map.empty;
  }

(* This function returns the parent node and the direct gateway *)
let resolve_opt t target_net gw =
  match Prefix_trie.find_opt t.trie (ip_to_bits gw) with
  | None -> 
    (* This route is not resolvable because its gw is not reachable. *)
    None
  | Some matched_pfx ->
    let node = Prefix_map.find matched_pfx t.map in
    let depended_routes = (Tree.value node)::(depended node) in
    let mutual route = 
      match route.gw with
      | None -> begin
        match route.iface with
        | None -> 
          Logs.err (fun m -> m "Some route has neither gw nor iface");
          assert false
        | Some _ -> false
      end
      | Some ip ->
        if not (Ipaddr.V4.Prefix.mem ip target_net) then false
        else
          let p = Prefix_trie.find t.trie (ip_to_bits ip) in
          Ipaddr.V4.Prefix.subset target_net p
    in
    match List.exists mutual depended_routes with
    | true -> 
      (* Mutual resolved *)
      None
    | false ->
      (* Can be resolved *)
      let direct_gw =
        let first = List.hd depended_routes in
        match first.gw with
        | Some v -> v
        | None ->
          if List.length depended_routes > 1 then
            let second = List.hd (List.tl depended_routes) in
            option_get second.gw
          else gw
      in
      Some (node, direct_gw)
;;

(* resolve without using cached data *)
let resolve_no_cache t target_net gw =
  let rec aux_resolve ip = 
    match Prefix_trie.find_opt t.trie (ip_to_bits ip) with
    | None -> 
      (* There is no route matching this ip. *)
      None
    | Some matched_pfx ->
      let node = Prefix_map.find matched_pfx t.map in
      match node.value.gw with
      | Some next_ip -> begin
        if not (Ipaddr.V4.Prefix.mem next_ip target_net) then 
          match node.value.iface with
          | None -> aux_resolve next_ip
          | Some _ -> Some next_ip
        else
          let p = Prefix_trie.find t.trie (ip_to_bits next_ip) in
          if Ipaddr.V4.Prefix.subset target_net p then
            (* Mutual resolved *)
            None
          else 
            match node.value.iface with
            | None -> aux_resolve next_ip
            | Some _ -> Some next_ip
      end
      | None -> begin
        match node.value.iface with
        | Some v -> Some ip
        | None -> 
          (* Error case: One route has neither iface nor gw. *)
          Logs.err (fun m -> m "One route has neither iface nor gw.");
          assert false
      end
  in
  match aux_resolve gw with
  | None -> None
  | Some direct_gw ->
    let parent_node = 
      let first_matched = Prefix_trie.find t.trie (ip_to_bits gw) in
      Prefix_map.find first_matched t.map
    in
    Some (parent_node, direct_gw)
;;

let resolve t net gw = 
  match resolve_opt t net gw with
  | None -> 
    Logs.err (fun m -> m "Resolve fails.");
    raise Not_found
  | Some v -> v
;;

let resolvable t net gw = resolve_opt t net gw <> None    

let add_static t net gw iface = 
  let route = { 
    static = true;
    net; gw; iface; 
  } in
  let node = Tree.create None [] route in
  let trie = Prefix_trie.set t.trie (prefix_to_bits net) net in
  let map = Prefix_map.add net node t.map in
  { trie; map }
;;

let is_static t net =
  match Prefix_map.find_opt net t.map with
  | None -> 
    Logs.err (fun m -> m "Route not found");
    assert false
  | Some node -> (Tree.value node).static
;;

(* Warning: do not use this on static routes. *)
let direct_gw node =
  let depended_routes = depended node in
  let first = List.hd depended_routes in
  match first.gw with
  | Some v -> v
  | None ->
    if List.length depended_routes > 1 then
      let second = List.hd (List.tl depended_routes) in
      option_get second.gw
    else option_get node.value.gw
;;

(* Warning: would not add the route if it is not resolvable. *)
let add t target_net gw = 
  match resolve_opt t target_net gw with
  | None -> 
    (* This route is not resolvable. *)
    t
  | Some (new_parent_node, new_direct_gw) -> 
    match Prefix_map.find_opt target_net t.map with
    | Some node ->
      (* This prefix exists already, this is a replace operation. *)
      
      if node.value.static then 
        (* The existing route is a route from kernel talbe. Prefer that for now. *)
        t
      else
        let old_direct_gw = direct_gw node in

        (* Remove link from old parent *)
        Tree.rm_child_v (option_get node.parent) node.value;

        (* Update link to new parent *)
        node.parent <- Some new_parent_node;

        (* Add link from new parent *)
        Tree.add_child new_parent_node node;

        (* Update value *)
        let new_route = { 
          static = false;
          net = target_net; 
          gw = Some gw;
          iface = None;
        } in
        node.value <- new_route;

        t
    | None ->
      (* This prefix does not exist before, this is an insertion. *)
      let tmp = Ipaddr.V4.Prefix.network target_net in
      match Prefix_trie.find_opt t.trie (ip_to_bits tmp) with
      | None ->
        (* This route brand new route. *)
        let route = { 
          static = false;
          net = target_net; 
          gw = Some gw;
          iface = None;
        } in
        let node = Tree.add_child_v new_parent_node route in
        let new_map = Prefix_map.add target_net node t.map in
        let new_trie = Prefix_trie.set t.trie (prefix_to_bits target_net) target_net in

        { trie = new_trie; map = new_map; }
      | Some supnet ->
        (* This 'net' is a subnet of some existing net. *)
        let supnet_node = Prefix_map.find supnet t.map in

        let route = { 
          static = false;
          net = target_net; 
          gw = Some gw;
          iface = None;
        } in
        let node = Tree.add_child_v new_parent_node route in
        let new_map = Prefix_map.add target_net node t.map in
        let new_trie = Prefix_trie.set t.trie (prefix_to_bits target_net) target_net in
        let new_t = { trie = new_trie; map = new_map; } in

        let depend child_node =  
          let r = child_node.value in
          Ipaddr.V4.Prefix.mem (option_get r.gw) target_net
        in
        
        (* l1 is the set of prefixes that depend on the new route *)
        (* l2 is the set of prefixes that depend on the old route *)
        let l1, l2 = List.partition depend supnet_node.children in
        node.children <- l1;
        List.iter (fun n -> n.parent <- Some node) l1;

        supnet_node.children <- l2;
        
        new_t
;;

let remove t target_net = 
  match Prefix_map.find_opt target_net t.map with
  | None -> 
    (* The prefix does not exist yet *)
    t, []
  | Some node ->     
    let new_map = Prefix_map.remove target_net t.map in
    let new_trie = Prefix_trie.unset t.trie (prefix_to_bits target_net) in
    let new_t = { trie = new_trie; map = new_map; } in
    
    (* remove links *)
    Tree.rm_child_v (option_get node.parent) node.value;
    List.iter (fun c -> c.parent <- None) node.children;

    let dependent_routes = dependents node in

    (* find those that are invalid after this route is removed *)
    let f (t, wd) route = 
      match resolve_no_cache t route.net (option_get route.gw) with
      | None -> 
        let new_map = Prefix_map.remove route.net t.map in
        let new_trie = Prefix_trie.unset t.trie (prefix_to_bits route.net) in
        let new_t = { trie = new_trie; map = new_map; } in

        Tree.rm_child_v (option_get node.parent) node.value;
        List.iter (fun c -> c.parent <- None) node.children;

        (new_t, route.net::wd)
      | Some (new_parent_node, new_direct_gw) ->
        let node = Prefix_map.find route.net t.map in
        
        let () = 
          match node.parent with
          | None -> ()
          | Some p -> Tree.rm_child_v p node.value
        in
        node.parent <- Some new_parent_node;

        (t, wd)
    in
    List.fold_left f (new_t, [ target_net ]) dependent_routes
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
  | Some (parent_node, gw) ->
    assert (gw = ip4);

  (* Mutual resolved *)
  let pfx5 = Ipaddr.V4.(Prefix.make 24 (of_string_exn "10.10.20.0")) in
  assert (resolve_opt t pfx5 ip5 = None);

  (* No match *)
  let pfx6 = Ipaddr.V4.(Prefix.make 16 (of_string_exn "50.50.0.0")) in
  let ip6 = Ipaddr.V4.of_string_exn "10.0.0.253" in
  assert (resolve_opt t pfx6 ip6 = None);

  (* False mutual resolved *)
  let t = add_static t pfx4 (Some ip5) None in
  let pfx9 = Ipaddr.V4.(Prefix.make 8 (of_string_exn "20.0.0.0")) in
  let ip9 = Ipaddr.V4.of_string_exn "40.40.0.1" in
  assert (resolvable t pfx9 ip9);

  (* Test add/replace/remove *)
  let t = empty in
  let t = add_static t pfx1 None (Some iface1) in

  let depended_v node = 
    List.map (fun r -> r.net) (depended node)
  in

  let dependents_v node =
    List.map (fun r -> r.net) (dependents node)
  in

  (* normal insert *)
  let t = add t pfx3 ip1 in
  assert (cardinal t = 2);
  assert (Prefix_map.mem pfx3 t.map);
  assert (Prefix_trie.mem t.trie (prefix_to_bits pfx3));
  let node = Prefix_map.find pfx3 t.map in
  assert (node.value.net = pfx3);
  assert (node.value.gw = Some ip1);
  assert (node.value.iface = None);

  assert (dependents_v node = []);
  assert (depended_v node = [ pfx1 ]);
  let node = Prefix_map.find pfx1 t.map in
  assert (dependents_v node = [ pfx3 ]);

  let t = add t pfx4 ip3 in
  let ip7 = Ipaddr.V4.of_string_exn "20.20.1.1" in
  let t = add t pfx6 ip7 in
  let node = Prefix_map.find pfx3 t.map in
  assert (List.length (dependents_v node) = 2);
  assert (List.mem pfx4 (dependents_v node));
  assert (List.mem pfx6 (dependents_v node));
  let node = Prefix_map.find pfx1 t.map in
  assert (List.length (dependents node) = 3);
  assert (List.mem pfx4 (dependents_v node));
  assert (List.mem pfx6 (dependents_v node));
  
  (* insert a sub route *)
  let pfx7 = Ipaddr.V4.(Prefix.make 24 (of_string_exn "20.20.1.0")) in  
  let t = add t pfx7 ip1 in
  assert (cardinal t = 5);
  assert (Prefix_map.mem pfx7 t.map);
  assert (Prefix_trie.mem t.trie (prefix_to_bits pfx7));
  let node = Prefix_map.find pfx3 t.map in
  assert (List.length (dependents_v node) = 1);
  assert (List.mem pfx4 (dependents_v node));
  let node = Prefix_map.find pfx7 t.map in
  assert (List.length (dependents node) = 1);
  assert (List.mem pfx6 (dependents_v node));
  let node = Prefix_map.find pfx1 t.map in
  assert (List.length (dependents node) = 4);

  (* Replace a non-depended route *)
  let t = add t pfx4 ip7 in
  let node = Prefix_map.find pfx4 t.map in
  assert (node.value.gw = Some ip7);
  assert ((dependents_v node) = []);
  assert (depended_v node = [ pfx1; pfx7; ]);
  let node = Prefix_map.find pfx3 t.map in
  assert (List.length (dependents node) = 0);
  let node = Prefix_map.find pfx7 t.map in
  assert (List.length (dependents node) = 2);
  assert (List.mem pfx4 (dependents_v node));
  assert (List.mem pfx6 (dependents_v node));
  let node = Prefix_map.find pfx1 t.map in
  assert (List.length (dependents node) = 4);

  (* Replace a depended route *)
  let pfx8 = Ipaddr.V4.(Prefix.make 24 (of_string_exn "60.60.0.0")) in  
  let ip8 = Ipaddr.V4.of_string_exn "60.60.0.1" in
  let iface2 = "eth1" in
  let t = add_static t pfx8 None (Some iface2) in
  let t = add t pfx7 ip8 in
  assert (cardinal t = 6);
  let node = Prefix_map.find pfx7 t.map in
  assert (node.value.gw = Some ip8);
  assert (List.length (dependents node) = 2);
  assert (List.mem pfx4 (dependents_v node));
  assert (List.mem pfx6 (dependents_v node));
  let node = Prefix_map.find pfx4 t.map in
  assert (depended_v node = [pfx8; pfx7]);
  let node = Prefix_map.find pfx1 t.map in
  assert (List.length (dependents_v node) = 1);
  assert (List.mem pfx3 (dependents_v node));
  let node = Prefix_map.find pfx8 t.map in
  assert (List.length (dependents node) = 3);
  assert (List.mem pfx4 (dependents_v node));
  assert (List.mem pfx6 (dependents_v node));
  assert (List.mem pfx7 (dependents_v node));

  (* Insert a route colliding with kernel route *)
  let t = add t pfx8 ip1 in
  let node = Prefix_map.find pfx8 t.map in
  assert (node.value.gw = None);

  (* Withdraw a non-depended route *)
  let t, wd = remove t pfx3 in
  assert (wd = [ pfx3 ]);
  assert (cardinal t = 5);
  let node = Prefix_map.find pfx1 t.map in
  assert ((dependents_v node) = []);
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
  let node = Prefix_map.find pfx8 t.map in
  assert ((dependents_v node) = []);
;;