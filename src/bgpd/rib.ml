(* The underlying data store is a binary tree *)
module Prefix = Ipaddr.V4.Prefix

module Prefix_map = Map.Make(Prefix)

type update = {
  withdrawn: Prefix.t list;
  path_attrs: Bgp.path_attrs;
  nlri: Prefix.t list;
}

module Adj_rib = struct
  type db = Bgp.path_attrs Prefix_map.t

  type t = {
    peer: int;
    callback: update -> unit Lwt.t;
    mutable db: db;
  }

  let create peer callback : t = {
    peer;
    callback;
    db = Prefix_map.empty;
  }

  let update_db db { withdrawn; path_attrs; nlri } = 
    let wd = List.filter (fun pfx -> Prefix_map.mem pfx db) withdrawn in
    let after_wd =
      let f rib pfx = Prefix_map.remove pfx rib in
      List.fold_left f db wd
    in
    
    let after_insert = 
      let f rib pfx = Prefix_map.add pfx path_attrs rib in
      List.fold_left f after_wd nlri
    in

    let output = {
      withdrawn = wd;
      path_attrs;
      nlri
    } in

    (after_insert, output)
  ;;

  let handle_update t update = 
    let new_db, output = update_db t.db update in
    t.db <- new_db;
    t.callback output
  ;;

  let to_string t =
    let pfxs_str = 
      let f pfx pa acc = 
        let pfx_str = Printf.sprintf "%s | %s" (Ipaddr.V4.Prefix.to_string pfx) (Bgp.path_attrs_to_string pa) in
        List.cons pfx_str acc
      in 
      String.concat "\n" (Prefix_map.fold f t.db [])
    in

    Printf.sprintf "Adj_RIB \n Connection id: %d \n Prefixes: %s" t.peer pfxs_str
end

module Loc_rib = struct
  (* Logging *)
  let rib_log = Logs.Src.create ~doc:"Loc-ocamlRIB logging" "Loc-RIB"
  module Rib_log = (val Logs.src_log rib_log : Logs.LOG)

  (* int stores the asn *)
  type t = {
    mutable subs: Adj_rib.t list;
    mutable db: (Bgp.path_attrs * int) Prefix_map.t;
  }

  type signal = 
  | Update of (update * int)
  | Subscribe of Adj_rib.t
  | Unsubscribe of Adj_rib.t

  let is_subscribed t asn = 
    let open Adj_rib in
    List.exists (fun x -> x.peer = asn) t.subs
  ;;

  let invoke_callback t (update, asn) =
    let open Adj_rib in
    let filtered = List.filter (fun x -> not (x.peer = asn)) t.subs in 
    Lwt.join (List.map (fun rib -> Adj_rib.handle_update rib update) filtered)
  ;;

  let subscribe t rib = 
    let open Adj_rib in
    if (is_subscribed t rib.peer) then 
      Rib_log.warn (fun m -> m "Duplicated rib subscription for asn %d" rib.peer)
    else t.subs <- List.cons rib t.subs;
    Lwt.return_unit
  ;;

  let remove_assoc_pfxes db asn = 
    let f pfx (_, stored) (db, wd) =
      if asn = stored then (Prefix_map.remove pfx db, List.cons pfx wd)
      else (db, wd)
    in
    Prefix_map.fold f db (db, [])
  ;;

  let unsubscribe t rib =
    let open Adj_rib in
    if (not (List.exists (fun x -> rib.peer = x.peer) t.subs)) then begin
      Rib_log.warn (fun m -> m "No rib subscription for asn %d" rib.peer);
      Lwt.return_unit
    end
    else begin
      t.subs <- List.filter (fun x -> x.peer != rib.peer) t.subs;  
      let new_db, withdrawn = remove_assoc_pfxes t.db rib.peer in
      t.db <- new_db;
      let update = { withdrawn; path_attrs = []; nlri = [] } in
      invoke_callback t (update, rib.peer)
    end
  ;;
  
  let is_better x y = true

  let create = {
    subs = [];
    db = Prefix_map.empty;
  }

  let update_db db ({ withdrawn = in_wd; path_attrs; nlri = in_nlri }, asn) =
    (* This function is pure *)

    let (db_aft_wd, out_wd) =
      let f (db, wd) pfx = 
        let opt = Prefix_map.find_opt pfx db in
        match opt with
        | None -> db, wd
        | Some (_, stored) -> if (stored = asn) then (Prefix_map.remove pfx db, List.cons pfx wd) else db, wd
      in
      List.fold_left f (db, []) in_wd
    in

    let db_aft_insert, out_nlri = 
      let f (db, out_nlri) pfx = 
        let opt = Prefix_map.find_opt pfx db in
        match opt with
        | None -> (Prefix_map.add pfx (path_attrs, asn) db, List.cons pfx out_nlri)
        | Some (pa, stored) ->
          if stored = asn then (Prefix_map.add pfx (path_attrs, asn) db, List.cons pfx out_nlri)
          else if is_better path_attrs pa then (Prefix_map.add pfx (path_attrs, asn) db, List.cons pfx out_nlri)
          else db, out_nlri
      in
      List.fold_left f (db_aft_wd, []) in_nlri
    in

    let output = {
      withdrawn = out_wd;
      path_attrs;
      nlri = out_nlri
    }
    in 

    (db_aft_insert, output)
  ;;

  let handle_update t (update, asn) =
    let new_db, output = update_db t.db (update, asn) in
    t.db <- new_db;
    invoke_callback t (update, asn)
  ;;

  let handle_signal t = function
    | Subscribe rib -> subscribe t rib
    | Unsubscribe rib -> unsubscribe t rib
    | Update (update, asn) -> handle_update t (update, asn)
  ;;

  let to_string t = 
    let pfxs_str = 
      let f pfx (pa, id) acc = 
        let pfx_str = Printf.sprintf "%s | %d | %s" (Ipaddr.V4.Prefix.to_string pfx) id (Bgp.path_attrs_to_string pa) in
        List.cons pfx_str acc
      in 
      String.concat "\n" (Prefix_map.fold f t.db [])
    in

    let subs_str = 
      let open Adj_rib in
      let tmp = List.map (fun rib -> Printf.sprintf "%d" rib.peer) t.subs in
      String.concat " " tmp
    in
      
    Printf.sprintf "Loc-RIB \n Connections: %s \n Routes:  %s" subs_str pfxs_str
  ;;
end

