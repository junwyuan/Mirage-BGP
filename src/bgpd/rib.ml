open Bgp

(* The underlying data store is a binary tree *)
module Prefix = Ipaddr.V4.Prefix
module Prefix_map = Map.Make(Prefix)

type update = Bgp.update

module Adj_rib = struct
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

  let update_db { withdrawn; path_attrs; nlri } db  = 
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
    let new_db, output = update_db update t.db in
    t.db <- new_db;
    if output.nlri = [] && output.withdrawn = [] then 
      Lwt.return_unit
    else 
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

    Printf.sprintf "Adj_RIB \n Remote: %s \n Prefixes: %s" (Ipaddr.V4.to_string t.remote_id) pfxs_str
  ;;

  let size t = Prefix_map.cardinal t.db
end

module Id_map = Map.Make(Ipaddr.V4)

module Loc_rib = struct
  (* Logging *)
  let rib_log = Logs.Src.create ~doc:"Loc-ocamlRIB logging" "Loc-RIB"
  module Rib_log = (val Logs.src_log rib_log : Logs.LOG)

  type t = {
    local_asn: int32;
    mutable subs: Adj_rib.t Id_map.t;
    mutable db: (Bgp.path_attrs * Ipaddr.V4.t) Prefix_map.t;
  }

  type signal = 
    | Update of (update * Ipaddr.V4.t)
    | Subscribe of Adj_rib.t
    | Unsubscribe of Adj_rib.t

  let take pfxs len =
    let rec loop pfxs len acc =
      match pfxs with
      | [] -> (acc, [])
      | pfx::tl ->
        let mask = Ipaddr.V4.Prefix.bits pfx in
        let bs = pfxlen_to_bytes mask + 1 in
        if bs > len then 
          (acc, pfxs)
        else 
          loop tl (len - bs) (pfx::acc)
    in
    loop pfxs len []
  ;;

  let rec split pfxs len =
    let rec loop pfxs len acc = 
      let taken, rest = take pfxs len in
      match rest with
      | [] -> taken::acc
      | _ -> loop rest len (taken::acc)
    in
    loop pfxs len []
  ;;

  let split_update update =
    let rec loop ({ withdrawn; path_attrs; nlri } as u) acc = 
      if len_update_buffer u <= 4096 then u::acc
      else
        let len_fixed = 23 in
        let len_wd = len_pfxs_buffer withdrawn in

        if len_wd > 4096 - len_fixed then
          let taken, withdrawn = take withdrawn (4096 - len_fixed) in
          let update = { withdrawn = taken; path_attrs = []; nlri = [] } in
          loop { withdrawn; path_attrs; nlri } (update::acc)
        else
          let len_pa = len_path_attrs_buffer path_attrs in
          if len_wd + len_pa + len_fixed >= 4096 then
            let update = { withdrawn; path_attrs = []; nlri = [] } in
            loop { withdrawn; path_attrs; nlri } (update::acc)
          else if len_wd > 0 then
            let taken, nlri = take nlri (4096 - len_wd - len_pa - len_fixed) in
            let update = { withdrawn; path_attrs; nlri = taken } in
            loop { withdrawn = []; path_attrs; nlri } (update::acc)
          else
            let taken, nlri = take nlri (4096 - len_pa - len_fixed) in
            let update = { withdrawn = []; path_attrs; nlri = taken } in
            loop { withdrawn; path_attrs; nlri } (update::acc)
    in
    loop update []
  ;;

  let is_subscribed t rib = 
    let open Adj_rib in
    Id_map.mem rib.remote_id t.subs
  ;;

  let invoke_callback t (update, remote_id) =
    let open Adj_rib in
    Rib_log.debug (fun m -> m "checkpoint 5");
    let split_update = split_update update in
    Rib_log.debug (fun m -> m "checkpoint 6");
    let f k v acc = 
      match k = remote_id with
      | true -> acc
      | false ->
        let rec send_loop updates = 
          match updates with
          | [] -> Lwt.return_unit
          | u::tl ->
            let%lwt () = Adj_rib.handle_update v u in
            send_loop tl
        in
        (send_loop split_update)::acc
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

  let get_assoc_pfxes db remote_id = 
    let f acc (pfx, (_, stored_id)) =
      if remote_id = stored_id then pfx::acc else acc
    in
    List.fold_left f [] (Prefix_map.bindings db)
  ;;

  let remove_assoc_pfxes db pfxs = 
    let f acc pfx = Prefix_map.remove pfx acc in
    List.fold_left f db pfxs
  ;;

  let unsubscribe t rib =
    let open Adj_rib in
    match not (is_subscribed t rib) with
    | true ->
      Rib_log.warn (fun m -> m "No rib subscription for remote %s" 
                                (Ipaddr.V4.to_string rib.remote_id));
      Lwt.return_unit
    | false -> 

      Rib_log.debug (fun m -> m "checkpoint 1");

      (* Update subscribed ribs *)
      t.subs <- Id_map.remove rib.remote_id t.subs;
      (* Update db *)
      let withdrawn = get_assoc_pfxes t.db rib.remote_id in
      let new_db = remove_assoc_pfxes t.db withdrawn in

      Rib_log.debug (fun m -> m "checkpoint 2");

      t.db <- new_db;
      (* Send withdraw to other peers *)
      let update = { withdrawn; path_attrs = []; nlri = [] } in
      invoke_callback t (update, rib.remote_id)
  ;;
  
  let is_aspath_loop local_as segment_list =
    let f = function
      | Bgp.Asn_seq l -> List.mem local_as l
      | Bgp.Asn_set l -> List.mem local_as l
    in
    List.exists f segment_list
  ;;

  let get_aspath_len segment_list =
    let rec loop len = function
      | [] -> len
      | segment::tl -> match segment with
        | Bgp.Asn_seq l -> loop (len + List.length l) tl
        | Bgp.Asn_set l -> loop (len + 1) tl
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
    
  let find_aspath path_attrs =
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
    let as_path_1 = find_aspath path_attrs_1 in
    let as_path_2 = find_aspath path_attrs_2 in
    if (get_aspath_len as_path_1 < get_aspath_len as_path_2) then true
    else if (get_aspath_len as_path_1 > get_aspath_len as_path_2) then false
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

  

  let create local_asn = {
    local_asn;
    subs = Id_map.empty;
    db = Prefix_map.empty;
  }

  (* This function is pure *)
  let update_db local_as ({ withdrawn = in_wd; path_attrs; nlri = in_nlri }, peer_id) db =
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
    if in_nlri = [] || is_aspath_loop local_as (find_aspath path_attrs) then
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
    let new_db, output = update_db t.local_asn (update, remote_id) t.db in
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

  let size t = Prefix_map.cardinal t.db
end

