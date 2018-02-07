open Bgp
open Lwt.Infix

(* Design choices: To avoid dependency loop, I allow Loc-RIB to depend on In-RIB and out-RIB. *)
(* This does cost some inflxibility as the form of callback is fixed. *)

(* Logging *)
let rib_log = Logs.Src.create ~doc:"RIB logging" "RIB"
module Rib_log = (val Logs.src_log rib_log : Logs.LOG)

(* The underlying data store is a binary tree *)
module Prefix = Ipaddr.V4.Prefix
module Prefix_map = Map.Make(Prefix)
module Prefix_set = Set.Make(Prefix)
module Str_map = Map.Make(String)

type update = Bgp.update
let is_empty_update u = u.withdrawn = [] && u.nlri = [] 


module Adj_rib_in = struct
  type input = 
    | Push of update
    | Pull of Prefix.t
    | Stop
  
  
  type t = {
    remote_id: Ipaddr.V4.t;
    callback: update -> unit Lwt.t;
    db: Bgp.path_attrs Prefix_map.t;
    stream: input Lwt_stream.t;
    pf: input option -> unit;
  }

  let set_db new_db t = {
    remote_id = t.remote_id;
    callback = t.callback;
    db = new_db;
    stream = t.stream;
    pf = t.pf;
  }
    
  let update_db { withdrawn; path_attrs; nlri } db  = 
    let to_be_wd = List.filter (fun pfx -> Prefix_map.mem pfx db) withdrawn in

    let db_after_wd =
      let f rib pfx = Prefix_map.remove pfx rib in
      List.fold_left f db to_be_wd
    in
    
    let db_after_insert = 
      let f rib pfx = Prefix_map.add pfx path_attrs rib in
      List.fold_left f db_after_wd nlri
    in

    let out_update = {
      withdrawn = to_be_wd;
      path_attrs;
      nlri
    } in

    (db_after_insert, out_update)
  ;;

  let rec handle_loop t = 
    Lwt_stream.get t.stream >>= function
    | None -> Lwt.return_unit
    | Some input -> begin
      match input with
      | Push update -> 
        let new_db, out_update = update_db update t.db in
        let new_t = set_db new_db t in
        if is_empty_update out_update then handle_loop new_t
        else 
          (* Handle the callback before handling next input request. This guarantees update message ordering *)
          t.callback out_update
          >>= fun () ->
          handle_loop new_t
      | Pull pfx -> begin
        match Prefix_map.find_opt pfx t.db with
        | None -> handle_loop t
        | Some path_attrs ->
          let update = {
            withdrawn = [];
            path_attrs;
            nlri = [ pfx ];
          } in
          t.callback update
          >>= fun () ->
          handle_loop t
      end
      | Stop -> Lwt.return_unit
    end
  ;;

  let create remote_id callback : t = 
    (* Construct the data structure *)
    let stream, pf = Lwt_stream.create () in
    let db = Prefix_map.empty in
    let t = {
      remote_id; callback; db;
      stream; pf;
    } in
    
    (* Spawn event handle loop *)
    let _ = handle_loop t in

    t
  ;;

  let input t input = 
    t.pf (Some input);
    Lwt.return_unit
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


module Adj_rib_out = struct
  type input = 
    | Push of update
    | Stop

  type t = {
    remote_id: Ipaddr.V4.t;
    callback: update -> unit Lwt.t;
    past_routes: Prefix_set.t;
    stream: input Lwt_stream.t;
    pf: input option -> unit;
  }

  let set_past_routes pr t = {
    remote_id = t.remote_id;
    callback = t.callback;
    past_routes = pr;
    stream = t.stream;
    pf = t.pf;
  }

  let build_db rev_updates =
    (* The argument are updates in reverse time order. The latest can first. *)

    let f (banned, db) u =
      (* Insert the newest paths into db *)
      let allowed = List.filter (fun pfx -> not (Prefix_set.mem pfx banned)) u.nlri in
      let new_db = 
        let f db pfx = 
          match Prefix_map.mem pfx db with
          | true -> 
            (* There is a newer route present *)
            db
          | false ->
            (* This is the newest route *)
            Prefix_map.add pfx u.path_attrs db
        in
        List.fold_left f db allowed
      in

      (* Update the banned list *)
      (* A route is banned if it has been withdrawn. All previous advertisements should be ignored. *)
      let new_banned = List.fold_left (fun acc pfx -> Prefix_set.add pfx acc) banned u.withdrawn in
      (new_banned, new_db)
    in
    List.fold_left f (Prefix_set.empty, Prefix_map.empty) rev_updates
  ;;

  let group db =
    let f (attr_map, pfx_map) (pfx, attrs) =
      (* Generate an id for the attrs *)
      let attr_id = path_attrs_to_string attrs in
      
      match Str_map.mem attr_id pfx_map with
      | true -> 
        let l = Str_map.find attr_id pfx_map in
        attr_map, Str_map.add attr_id (pfx::l) pfx_map
      | false ->
        Str_map.add attr_id attrs attr_map, Str_map.add attr_id [pfx] pfx_map 
    in
    let attr_map, pfx_map = List.fold_left f (Str_map.empty, Str_map.empty) (Prefix_map.bindings db) in
    List.map (fun (id, pfx_list) -> (Str_map.find id attr_map, pfx_list)) (Str_map.bindings pfx_map)
  ;;

  let take_n l n =
    let rec loop l n acc =
      if n = 0 then acc, l
      else 
        match l with
        | [] -> acc, []
        | hd::tl -> loop tl (n-1) (hd::acc)
    in
    let tmp, rest = loop l n [] in
    List.rev tmp, rest
  ;;

  let rec split pfxs len =
    let rec loop pfxs len acc = 
      match pfxs with
      | [] -> acc
      | _ ->
        let taken, rest = take_n pfxs len in
        loop rest len (taken::acc)
    in
    loop pfxs len []
  ;;
    
  let rec handle_loop t = 
    Lwt_stream.peek t.stream >>= function
    | None -> Lwt.return_unit
    | Some _ -> 
      let inputs = Lwt_stream.get_available t.stream in
      match List.exists (fun i -> i = Stop) inputs with
      | true -> 
        (* Dump any unhandled inputs when the RIB is stopped *)
        Lwt.return_unit
      | false ->
        let rev_updates = 
          let f acc i = match i with
            | Stop -> acc
            | Push u -> u::acc
          in
          List.fold_left f [] inputs
        in

        (* Construct the db of updates in reverse time order *)
        let banned, db = build_db rev_updates in

        (* Compute the withdrawn and insert set *)
        let wd_set = Prefix_set.inter banned t.past_routes in
        let insert_set = Prefix_set.of_list (List.map (fun (pfx, _) -> pfx) (Prefix_map.bindings db)) in
        
        (* Transform the db into groups based on path_attrs *)
        let attr_and_pfxs = group db in

        (* Generate updates *)
        let len_fixed = 23 in
        let wd_updates =
          let tmp = split (Prefix_set.elements wd_set) ((4096 - len_fixed) / 5) in 
          List.map (fun pfxs -> { withdrawn = pfxs; path_attrs = []; nlri = [] }) tmp
        in
        let insert_updates =
          let f acc (path_attrs, pfxs) = 
            let len_attr = len_path_attrs_buffer path_attrs in
            let tmp = split pfxs ((4096 - len_fixed - len_attr) / 5) in
            let l = List.map (fun pfxs -> { withdrawn = []; path_attrs; nlri = pfxs }) tmp in
            l @ acc
          in
          List.fold_left f [] attr_and_pfxs
        in

        (* To guarantee the equivalence of operation, must send withdrawn first. *)
        let updates = wd_updates @ insert_updates in
        let rec send_loop = function
          | [] -> Lwt.return_unit
          | u::tl -> t.callback u >>= fun () -> send_loop tl
        in
        send_loop updates >>= fun () ->
        
        (* Decision choice: must send all previous updates before handling next batch. *)
        (* Update data structure *)
        let n_routes = Prefix_set.union (Prefix_set.diff t.past_routes wd_set) insert_set in
        let n_t = set_past_routes n_routes t in

        (* Here is the pull timer, hard-coded 50ms *)
        OS.Time.sleep_ns (Duration.of_ms 50)
        >>= fun () ->

        handle_loop n_t
  ;;


  let create remote_id callback : t = 
    (* Initiate data structure *)
    let stream, pf = Lwt_stream.create () in
    let past_routes = Prefix_set.empty in
    let t = {
      remote_id; callback; past_routes;
      stream; pf;
    } in

    (* Spawn handling loop *)
    let _ = handle_loop t in

    t
  ;;
end


module Loc_rib = struct
  module Id_map = Map.Make(Ipaddr.V4)

  type t = {
    local_asn: int32;
    local_id: Ipaddr.V4.t;
    mutable subs: Adj_rib_out.t Id_map.t;
    mutable db: (Bgp.path_attrs * Ipaddr.V4.t) Prefix_map.t;
  }


  type signal = 
    | Update of (update * Ipaddr.V4.t)
    | Subscribe of Adj_rib_out.t
    | Unsubscribe of Adj_rib_out.t


  let create config = 
    let open Config_parser in
    {
      local_id =  config.local_id;
      local_asn = config.local_asn;
      subs = Id_map.empty;
      db = Prefix_map.empty;
    }
  ;;


  let invoke_callback t (update, remote_id) =
    let open Adj_rib_out in
    let f k v acc = 
      match k = remote_id with
      | true -> acc
      | false -> (Adj_rib_out.handle_update v update)::acc
    in
    Lwt.join (Id_map.fold f t.subs [])
  ;;

  let is_subscribed t rib = 
    let open Adj_rib_out in
    Id_map.mem rib.remote_id t.subs
  ;;

  let subscribe t rib = 
    let open Adj_rib_out in
    match (is_subscribed t rib) with
    | true ->
      Rib_log.warn (fun m -> m "Duplicated rib subscription for remote %s" 
                            (Ipaddr.V4.to_string rib.remote_id));
      Lwt.return_unit        
    | false ->
      t.subs <- Id_map.add rib.remote_id rib t.subs;
      
      let f p (pa, _) acc = 
        let update = { withdrawn = []; path_attrs = pa; nlri = [p] } in
        List.cons (Adj_rib_out.handle_update rib update) acc
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
    let open Adj_rib_out in
    match not (is_subscribed t rib) with
    | true ->
      Rib_log.warn (fun m -> m "No rib subscription for remote %s" 
                                (Ipaddr.V4.to_string rib.remote_id));
      Lwt.return_unit
    | false -> 

      (* Update subscribed ribs *)
      t.subs <- Id_map.remove rib.remote_id t.subs;
      (* Update db *)
      let withdrawn = get_assoc_pfxes t.db rib.remote_id in
      let new_db = remove_assoc_pfxes t.db withdrawn in

      t.db <- new_db;
      (* Send withdraw to other peers *)
      let update = { withdrawn; path_attrs = []; nlri = [] } in
      invoke_callback t (update, rib.remote_id)
  ;;
  
  let is_aspath_loop local_asn segment_list =
    let f = function
      | Bgp.Asn_seq l -> List.mem local_asn l
      | Bgp.Asn_set l -> List.mem local_asn l
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


  let append_aspath asn segments = 
    match segments with
    | [] -> [ Asn_seq [asn] ]
    | hd::tl -> match hd with
      | Asn_set _ -> (Asn_seq [asn])::segments
      | Asn_seq l -> (Asn_seq (asn::l))::tl
  ;;

  let update_aspath asn path_attrs = 
    match find_aspath path_attrs with
    | None ->
      Rib_log.warn (fun m -> m "MISSING AS PATH");
      As_path [Asn_seq [asn]]::path_attrs
    | Some aspath ->
      let appended = append_aspath asn aspath in
      let tmp = path_attrs_remove AS_PATH path_attrs in
      As_path appended::tmp
  ;;

  let update_nexthop ip path_attrs =
    let tmp = path_attrs_remove NEXT_HOP path_attrs in
    (Next_hop ip)::tmp
  ;;

  let find_origin path_attrs = 
    match Bgp.find_origin path_attrs with
    | None ->
      Rib_log.err (fun m -> m "MISSING ORIGIN");
      failwith "MISSING ORIGIN"
    | Some v -> v
  ;;

  let find_aspath path_attrs = 
    match Bgp.find_aspath path_attrs with
    | None -> 
      Rib_log.err (fun m -> m "MISSING AS PATH");
      failwith "MISSING ASPATH"
    | Some v -> v
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


  (* This function is pure *)
  let update_db local_id local_asn ({ withdrawn = in_wd; path_attrs; nlri = in_nlri }, peer_id) db =

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
    if in_nlri = [] || is_aspath_loop local_asn (find_aspath path_attrs) then begin
      let out_update = { withdrawn = out_wd; path_attrs = []; nlri = [] } in
      (db_aft_wd, out_update)
    end else
      let updated_path_attrs = 
        path_attrs 
        |> update_nexthop local_id 
        |> update_aspath local_asn
      in

      let db_aft_insert, out_nlri = 
        let f (db, out_nlri) pfx = 
          let opt = Prefix_map.find_opt pfx db in
          match opt with
          | None -> 
            (Prefix_map.add pfx (updated_path_attrs, peer_id) db, pfx::out_nlri)
          | Some (stored_pattrs, src_id) ->
            (* If from the same peer, replace without compare *)
            if src_id = peer_id then 
              (Prefix_map.add pfx (updated_path_attrs, peer_id) db, pfx::out_nlri)
            else if tie_break (updated_path_attrs, peer_id) (stored_pattrs, src_id) then begin
              (Prefix_map.add pfx (updated_path_attrs, peer_id) db, pfx::out_nlri)
            end
            else begin
              db, out_nlri
            end
        in
        List.fold_left f (db_aft_wd, []) in_nlri
      in

      let out_update = {
        withdrawn = out_wd;
        path_attrs = updated_path_attrs;
        nlri = out_nlri
      }
      in 

      (db_aft_insert, out_update)
  ;;

  let handle_update t (update, remote_id) =
    let new_db, output = update_db t.local_id t.local_asn (update, remote_id) t.db in
    t.db <- new_db;
    if output.nlri = [] && output.withdrawn = [] then Lwt.return_unit
    else invoke_callback t (output, remote_id)
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
      let open Adj_rib_in in
      let tmp = List.map (fun (x, _) -> Ipaddr.V4.to_string x) (Id_map.bindings t.subs) in
      String.concat " " tmp
    in
      
    Printf.sprintf "Loc-RIB \n Connections: %s \n Routes:  %s" subs_str pfxs_str
  ;;

  let size t = Prefix_map.cardinal t.db
end





