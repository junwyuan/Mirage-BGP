(* The underlying data store is a binary tree *)
module Prefix = Ipaddr.V4.Prefix
module Prefix_map = Map.Make(Prefix)

type update = {
  withdrawn: Prefix.t list;
  path_attrs: Bgp.path_attrs;
  nlri: Prefix.t list;
}

module Adj_rib = struct

(* : sig
  type t = {
    remote_id: Ipaddr.V4.t;
    callback: update -> unit Lwt.t;
    mutable db: Bgp.path_attrs Prefix_map.t;
  }

  val create : Ipaddr.V4.t -> (update -> unit Lwt.t) -> t
  val handle_update : t -> update -> unit Lwt.t
  val to_string : t -> string
  val size : t -> int
end = struct *)

  type t = {
    remote_id: Ipaddr.V4.t;
    callback: update -> unit Lwt.t;
    mutable db: Bgp.path_attrs Prefix_map.t;
  }

  let create remote_id callback : t = {
    remote_id;
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
    if output.nlri = [] && output.withdrawn = [] then Lwt.return_unit
    else t.callback output
  ;;

  let to_string t =
    let pfxs_str = 
      let f pfx pa acc = 
        let pfx_str = Printf.sprintf "%s | %s" (Ipaddr.V4.Prefix.to_string pfx) (Bgp.path_attrs_to_string pa) in
        List.cons pfx_str acc
      in 
      String.concat "\n" (Prefix_map.fold f t.db [])
    in

    Printf.sprintf "Adj_RIB \n Remote: %s \n Prefixes: %s" (Ipaddr.V4.to_string t.remote_id) pfxs_str
  ;;

  let size t = 
    let f _ _ acc = acc + 1 in
    Prefix_map.fold f t.db 0
  ;;
end

module Util = struct

end

module Id_map = Map.Make(Ipaddr.V4)

module Loc_rib = struct
(* : sig
  type t = {
    local_as: int32;
    mutable subs: Adj_rib.t Id_map.t;
    mutable db: (Bgp.path_attrs * Ipaddr.V4.t) Prefix_map.t;
  }

  type signal = 
    | Update of (update * Ipaddr.V4.t)
    | Subscribe of Adj_rib.t
    | Unsubscribe of Adj_rib.t

  val create : int32 -> t
  val handle_signal : t -> signal -> unit Lwt.t
  val to_string : t -> string
  val size : t -> int
end = struct *)
  (* Logging *)
  let rib_log = Logs.Src.create ~doc:"Loc-ocamlRIB logging" "Loc-RIB"
  module Rib_log = (val Logs.src_log rib_log : Logs.LOG)

  type t = {
    local_as: int32;
    mutable subs: Adj_rib.t Id_map.t;
    mutable db: (Bgp.path_attrs * Ipaddr.V4.t) Prefix_map.t;
  }

  type signal = 
    | Update of (update * Ipaddr.V4.t)
    | Subscribe of Adj_rib.t
    | Unsubscribe of Adj_rib.t

  let is_subscribed t rib = 
    let open Adj_rib in
    Id_map.mem rib.remote_id t.subs
  ;;

  let invoke_callback t (update, remote_id) =
    let open Adj_rib in
    let f k v acc = 
      match k = remote_id with
      | true -> acc
      | false -> List.cons (Adj_rib.handle_update v update) acc
    in
    Lwt.join (Id_map.fold f t.subs [])
  ;;

  let subscribe t rib = 
    let open Adj_rib in
    match (is_subscribed t rib) with
    | true ->
      Rib_log.warn (fun m -> m "Duplicated rib subscription for remote %s" 
                            (Ipaddr.V4.to_string rib.remote_id));
      Lwt.return_unit        
    | false ->
      t.subs <- Id_map.add rib.remote_id rib t.subs;
      
      let f p (pa, _) acc = 
        let update = { withdrawn = []; path_attrs = pa; nlri = [p] } in
        List.cons (Adj_rib.handle_update rib update) acc
      in
      Lwt.join (Prefix_map.fold f t.db [])
  ;;

  let remove_assoc_pfxes db remote_id = 
    let f pfx (_, stored_id) (db, wd) =
      if remote_id = stored_id then (Prefix_map.remove pfx db, List.cons pfx wd)
      else (db, wd)
    in
    Prefix_map.fold f db (db, [])
  ;;

  let unsubscribe t rib =
    let open Adj_rib in
    match not (is_subscribed t rib) with
    | true ->
      Rib_log.warn (fun m -> m "No rib subscription for remote %s" 
                                (Ipaddr.V4.to_string rib.remote_id));
      Lwt.return_unit
    | false -> 
      (* Update subscribed ribs *)
      t.subs <- Id_map.remove rib.remote_id t.subs;
      (* Update db *)
      let new_db, withdrawn = remove_assoc_pfxes t.db rib.remote_id in
      t.db <- new_db;
      (* Send withdraw to other peers *)
      let update = { withdrawn; path_attrs = []; nlri = [] } in
      invoke_callback t (update, rib.remote_id)
  ;;
  
  let is_aspath_loop local_as segment_list =
    let f = function
      | Bgp.Seq l -> List.mem local_as l
      | Bgp.Set l -> List.mem local_as l
    in
    List.exists f segment_list
  ;;

  let get_aspath_len segment_list =
    let rec loop len = function
      | [] -> len
      | segment::tl -> match segment with
        | Bgp.Seq l -> loop (len + List.length l) tl
        | Bgp.Set l -> loop (len + 1) tl
    in
    loop 0 segment_list
  ;;

  let find_origin path_attrs =
    let rec loop = function
      | [] -> 
        Rib_log.err (fun m -> m "Missing ORIGIN");
        failwith "Missing ORIGIN"
      | hd::tl -> match hd with
        | (_, Bgp.Origin v) -> v
        | _ -> loop tl
    in
    loop path_attrs
  ;;
    
  let find_as_path path_attrs =
    let rec loop = function
      | [] -> 
        Rib_log.err (fun m -> m "Missing AS PATH");
        failwith "Missing AS PATH"
      | hd::tl -> match hd with
        | (_, Bgp.As_path v) -> v
        | (_, Bgp.As4_path v) -> v 
        | _ -> loop tl
    in
    loop path_attrs
  ;;

  (* Output true: 1st argument is more preferable, output false: 2nd argument is more preferable *)
  let tie_break (path_attrs_1, peer_id_1) (path_attrs_2, peer_id_2) = 
    let as_path_1 = find_as_path path_attrs_1 in
    let as_path_2 = find_as_path path_attrs_2 in
    if (get_aspath_len as_path_1 > get_aspath_len as_path_2) then true
    else if (get_aspath_len as_path_1 < get_aspath_len as_path_2) then false
    else begin
      let origin_1 = find_origin path_attrs_1 in
      let origin_2 = find_origin path_attrs_2 in
      if origin_1 = Bgp.EGP && origin_2 = Bgp.IGP then true
      else if origin_1 = Bgp.IGP && origin_2 = Bgp.EGP then false
      else begin
        if Ipaddr.V4.compare peer_id_1 peer_id_2 = -1 then true else false
      end
    end
  ;;

  let create local_as = {
    local_as;
    subs = Id_map.empty;
    db = Prefix_map.empty;
  }

  (* This function is pure *)
  let update_db local_as db ({ withdrawn = in_wd; path_attrs; nlri = in_nlri }, peer_id) =
    (* Withdrawn from Loc-RIB *)
    let (db_aft_wd, out_wd) =
      let f (db, out_wd) pfx = 
        let opt = Prefix_map.find_opt pfx db in
        match opt with
        | None -> db, out_wd
        | Some (_, stored) -> if (stored = peer_id) then (Prefix_map.remove pfx db, List.cons pfx out_wd) else db, out_wd
      in
      List.fold_left f (db, []) in_wd
    in

    (* If the advertised path is looping, don't install any new routes *)
    if is_aspath_loop local_as (find_as_path path_attrs) then
      let out_update = { withdrawn = out_wd; path_attrs = []; nlri = [] } in
      (db_aft_wd, out_update)
    else
      let db_aft_insert, out_nlri = 
        let f (db, out_nlri) pfx = 
          let opt = Prefix_map.find_opt pfx db in
          match opt with
          | None -> (Prefix_map.add pfx (path_attrs, peer_id) db, List.cons pfx out_nlri)
          | Some (stored_pattrs, src_id) ->
            (* If from the same peer, replace without compare *)
            if src_id = peer_id then 
              (Prefix_map.add pfx (path_attrs, peer_id) db, List.cons pfx out_nlri)
            else if tie_break (path_attrs, peer_id) (stored_pattrs, src_id) then 
              (Prefix_map.add pfx (path_attrs, peer_id) db, out_nlri)
            else db, out_nlri
        in
        List.fold_left f (db_aft_wd, []) in_nlri
      in

      let out_update = {
        withdrawn = out_wd;
        path_attrs;
        nlri = out_nlri
      }
      in 

      (db_aft_insert, out_update)
  ;;

  let handle_update t (update, remote_id) =
    let new_db, output = update_db t.local_as t.db (update, remote_id) in
    t.db <- new_db;
    if output.nlri = [] && output.withdrawn = [] then Lwt.return_unit
    else invoke_callback t (update, remote_id)
  ;;

  let handle_signal t = function
    | Subscribe rib -> subscribe t rib
    | Unsubscribe rib -> unsubscribe t rib
    | Update (update, remote_id) -> handle_update t (update, remote_id)
  ;;

  let to_string t = 
    let pfxs_str = 
      let f pfx (pa, id) acc = 
        let pfx_str = Printf.sprintf "%s | %s | %s" (Ipaddr.V4.Prefix.to_string pfx) 
                                (Ipaddr.V4.to_string id) (Bgp.path_attrs_to_string pa) in
        List.cons pfx_str acc
      in 
      String.concat "\n" (Prefix_map.fold f t.db [])
    in

    let subs_str = 
      let open Adj_rib in
      let tmp = List.map (fun (x, _) -> Ipaddr.V4.to_string x) (Id_map.bindings t.subs) in
      String.concat " " tmp
    in
      
    Printf.sprintf "Loc-RIB \n Connections: %s \n Routes:  %s" subs_str pfxs_str
  ;;

  let size t = 
    let f _ _ acc = acc + 1 in
    Prefix_map.fold f t.db 0
  ;;
end

