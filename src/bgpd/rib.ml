open Bgp
open Lwt.Infix

let option_get = function
  | None -> 
    Logs.debug (fun m -> m "option_get is called on None.");
    assert false
  | Some v -> v
;;



(* Design choices: To avoid dependency loop, I allow Loc-RIB to depend on In-RIB and out-RIB. *)
(* This does cost some inflxibility as the form of callback is fixed. *)
(* The interconnection between IN-RIB and Loc-RIB must be defined at initialisation in bgpd.ml *)

(* Design choices: Only routes within the Loc-RIB have updated path_attrs *)

(* Logging *)
let rib_log = Logs.Src.create ~doc:"RIB logging" "RIB"
module Rib_log = (val Logs.src_log rib_log : Logs.LOG)

(* The underlying data store is a binary tree *)
module Prefix = Ipaddr.V4.Prefix
module Prefix_map = Map.Make(Prefix)
module Ip_map = Map.Make(Ipaddr.V4)

module ID = struct
  type t = int
  let compare (a: t) (b: t) = a - b
  let create attrs = Hashtbl.hash attrs
end

module ID_map = Map.Make(ID)

module Dict = struct
  type 'a t = ('a * int) ID_map.t

  let add id attrs t = 
    match ID_map.find_opt id t with
    | None -> ID_map.add id (attrs, 1) t
    | Some (stored_attrs, c) -> ID_map.add id (stored_attrs, c + 1) t
  ;;

  let remove id t = 
    match ID_map.find_opt id t with
    | None -> t
    | Some (stored_attrs, c) ->
      if (c = 1) then 
        ID_map.remove id t 
      else
        ID_map.add id (stored_attrs, c - 1) t
  ;;

  let find_opt id t = 
    match ID_map.find_opt id t with
    | None -> None
    | Some (attrs, _c) -> Some attrs
  ;;

  let iter f t =
    let g k (v, c) = f k v in
    ID_map.iter g t
  ;;

  let find id t = 
    let attrs, _ = ID_map.find id t in
    attrs
  ;;

  let cardinal t = ID_map.cardinal t

  let mem id t = ID_map.mem id t

  let count id t = 
    match ID_map.find_opt id t with
    | None -> 0
    | Some (_, c) -> c
  ;;

  let empty = ID_map.empty
end

type update = Bgp.update
let is_empty_update u = u.withdrawn = [] && u.nlri = [] 

module Adj_rib_in = struct
  type input = 
    | Push of update
    | Pull of Prefix.t list
    | Stop
  
  type output = Ipaddr.V4.Prefix.t list * Ipaddr.V4.Prefix.t list * Bgp.path_attrs * int * int

  type t = {
    mutable running: bool;

    local_asn: int32;
    remote_id: Ipaddr.V4.t;
    iBGP: bool;
    
    callback: output list -> unit;
    
    mutable db: (ID.t * int) Prefix_map.t;
    mutable dict: path_attrs Dict.t;
    
    stream: input Lwt_stream.t;
    pf: input option -> unit;

    filter: Filter.route_map option;
  }

  let is_aspath_loop local_asn segment_list =
    let f = function
      | Bgp.Asn_seq l -> List.mem local_asn l
      | Bgp.Asn_set l -> List.mem local_asn l
    in
    List.exists f segment_list
  ;;
    
  let update_in_db { withdrawn; path_attrs; nlri } local_asn filter_opt ibgp db dict = 
    let out_wd, db_aft_wd, dict_aft_wd =
      let f (wd, db, dict) pfx = 
        match Prefix_map.find_opt pfx db with
        | None -> (wd, db, dict)
        | Some (attr_id, _w) -> (pfx::wd, Prefix_map.remove pfx db, Dict.remove attr_id dict)
      in
      List.fold_left f ([], db, dict) withdrawn
    in

    match is_aspath_loop local_asn (find_as_path path_attrs) with
    | true ->
      (* AS PATH LOOP *)
      let f (wd, db, dict) pfx = 
        match Prefix_map.find_opt pfx db with
        | None -> (wd, db, dict)
        | Some (attr_id, _w) -> (pfx::wd, Prefix_map.remove pfx db, Dict.remove attr_id dict)
      in
      let (wd, db, dict) = List.fold_left f ([], db, dict) nlri in
      (db, dict, [wd, [], path_attrs, 0, 0])
    | false -> begin
      match filter_opt with
      | None -> 
        let attr_id = ID.create path_attrs in

        (* Set weight *)
        let weight = 
          if ibgp then 
            match Bgp.find_local_pref path_attrs with
            | None -> 0
            | Some v -> Int32.to_int v
          else 0
        in

        let db_aft_ins, dict_aft_ins = 
          let f (db, dict) pfx =
            match Prefix_map.find_opt pfx db with
            | None ->
              (Prefix_map.add pfx (attr_id, weight) db, Dict.add attr_id path_attrs dict)
            | Some (stored, _w) ->
              let new_dict = Dict.add attr_id path_attrs dict |> Dict.remove stored in
              (Prefix_map.add pfx (attr_id, weight) db, new_dict)
          in
          List.fold_left f (db_aft_wd, dict_aft_wd) nlri
        in

        
        let out = 
          if out_wd = [] && nlri = [] then [] 
          else [out_wd, nlri, path_attrs, attr_id, weight] 
        in

        (db_aft_ins, dict_aft_ins, out)
      | Some filter ->
        let wd, ins, db_aft_ins, dict_aft_ins = 
          let f (wd, ins, db, dict) pfx =
            match Filter.apply filter pfx path_attrs with
            | None -> begin
              match Prefix_map.find_opt pfx db with
              | None -> (wd, ins, db, dict)
              | Some (s_attr_id, _w) ->
                (pfx::wd, ins, Prefix_map.remove pfx db, Dict.remove s_attr_id dict)
            end
            | Some (attrs, weight) ->
              let attr_id = ID.create attrs in
              match Prefix_map.find_opt pfx db with
              | None ->
                let tmp = (pfx, attrs, attr_id, weight) in
                (wd, tmp::ins, Prefix_map.add pfx (attr_id, weight) db, Dict.add attr_id path_attrs dict)
              | Some (stored, _w) ->
                let tmp = (pfx, attrs, attr_id, weight) in
                let new_dict = Dict.add attr_id path_attrs dict |> Dict.remove stored in
                (wd, tmp::ins, Prefix_map.add pfx (attr_id, weight) db, new_dict)
          in
          List.fold_left f (out_wd, [], db_aft_wd, dict_aft_wd) nlri
        in

        let out_wd = if wd <> [] then [(wd, [], [], 0, 0)] else [] in
        let out_ins = List.map (fun (pfx, attrs, attr_id, weight) -> ([], [pfx], attrs, attr_id, weight)) ins in

        let out = out_wd @ out_ins in
        (db_aft_ins, dict_aft_ins, out)
    end
  ;;

  let rec handle_loop t = 
    let in_rib_handle = function
      | None -> Lwt.return_unit
      | Some input -> match input with
        | Push update -> 
          let new_db, new_dict, out = update_in_db update t.local_asn t.filter t.iBGP t.db t.dict in
          t.db <- new_db;
          t.dict <- new_dict;

          (* Handle the callback before handling next input request. This guarantees update message ordering *)
          let () = t.callback out in
          handle_loop t
        | Pull pfx_list -> 
          let f acc pfx =
            match Prefix_map.find_opt pfx t.db with
            | None -> acc
            | Some (attr_id, w) ->
              let path_attrs = Dict.find attr_id t.dict in
              ([], [pfx], path_attrs, attr_id, w)::acc
          in
          let out = List.fold_left f [] pfx_list in
          let () = t.callback out in
          handle_loop t
        | Stop -> 
          Lwt.return_unit
    in
    Lwt_stream.get t.stream >>= fun input ->
    in_rib_handle input
  ;;

  let create local_asn remote_id iBGP callback filter : t = 
    (* Construct the data structure *)
    let stream, pf = Lwt_stream.create () in
    let db = Prefix_map.empty in
    let dict = Dict.empty in
    let t = {
      running = true;
      local_asn;
      remote_id; 
      iBGP;
      callback;
      db; dict;
      stream; pf;
      filter;
    } in
    
    (* Spawn event handle loop *)
    let _ = handle_loop t in

    t
  ;;

  let input t input = t.pf (Some input)
  
  let push_update t update = input t (Push update)
  
  let stop t = input t Stop
end


module Adj_rib_out = struct
  module Prefix_set = Set.Make(Prefix)

  type change = Prefix.t list * Prefix.t list * Bgp.path_attrs * int

  type input = 
    | Push of change
    | Sub of Ipaddr.V4.t * (update -> unit)
    | Stop

  type t = {
    id: int;

    (* States *)
    local_id: Ipaddr.V4.t;
    local_asn: int32;
    iBGP: bool;
    internal_transit: bool;
    filter_opt: Filter.route_map option;
    peer_group: bool;

    mutable callbacks: (update -> unit) Ip_map.t;
    mutable db: path_attrs Prefix_map.t;
    
    (* Queue *)
    stream: input Lwt_stream.t;
    pf: input option -> unit;
  }



  let prepend_aspath attrs asn =
    let append_aspath asn segments = 
      match segments with
      | [] -> [ Asn_seq [asn] ]
      | hd::tl -> match hd with
        | Asn_set _ -> (Asn_seq [asn])::segments
        | Asn_seq l -> (Asn_seq (asn::l))::tl
    in
    let segs = find_as_path attrs in
    set_as_path attrs (append_aspath asn segs)
  ;;

  (* GROUPING and REDUCTION *)
  let build_db rev_updates =
    (* rev_updates are updates in reverse time order. The latest come first. *)
    let aux_build_db (wd, ins, db) u =
      let delta_ins = 
        (* remove prefixes that will be withdrawn later in time *)
        let is_not_wd pfx = not (Prefix_set.mem pfx wd) in
        let aux = List.filter is_not_wd u.nlri in
        
        (* remove prefixes that will be replaced later in time *)
        let is_not_replaced pfx = not (Prefix_set.mem pfx ins) in
        List.filter is_not_replaced aux 
      in

      let new_ins = 
        let tmp = Prefix_set.of_list delta_ins in
        Prefix_set.union ins tmp
      in

      let new_wd =
        let delta_wd = 
          let is_not_inserted pfx = not (Prefix_set.mem pfx ins) in
          List.filter is_not_inserted u.withdrawn
        in
        let tmp = Prefix_set.of_list delta_wd in
        Prefix_set.union tmp wd
      in

      let new_db = 
        if delta_ins <> [] then
          let attr_id = ID.create u.path_attrs in
          match ID_map.find_opt attr_id db with
          | None ->
            ID_map.add attr_id (u.path_attrs, delta_ins) db
          | Some (attrs, pfxs) ->
            ID_map.add attr_id (attrs, delta_ins @ pfxs) db
        else db
      in

      (new_wd, new_ins, new_db)
    in
    
    List.fold_left aux_build_db (Prefix_set.empty, Prefix_set.empty, ID_map.empty) rev_updates 
  ;;

  (* This is not a standard List take operation. The result comes out in reverse order as in the standard implementation. *)
  let take l n =
    if List.length l <= n then (l, [])
    else
      let rec aux_take l n acc =
        if n = 0 then acc, l
        else match l with
          | [] -> acc, []
          | hd::tl -> aux_take tl (n-1) (hd::acc)
      in
      aux_take l n []
  ;;

  let split l len =
    let rec aux_split l len acc = 
      if List.length l > len then 
        let taken, rest = take l len in
        aux_split rest len (taken::acc)
      else if List.length l = len then (l::acc, [])
      else (acc, l)
    in
    aux_split l len []
  ;;

  let gen_updates wd db =
    let max_update_len = 4096 in
    let min_update_len = 23 in
    let pfx_len = 5 in
    
    (* Generate messages for each set of distinct path attributes *)
    let aux_gen_updates (wd, acc) (_id, (attrs, ins)) =
      let attrs_len = len_path_attrs_buffer attrs in
      let pfxs_list, rest = split ins ((max_update_len - min_update_len - attrs_len) / pfx_len) in
      let gen_ins_update ins = { withdrawn = []; path_attrs = attrs; nlri = ins } in
      
      let updates, rest_wd = 
        let partial = List.map gen_ins_update pfxs_list in
        if rest <> [] then
          let wd_num = (max_update_len - min_update_len - attrs_len - pfx_len * (List.length rest)) / pfx_len in
          let wd, rest_wd = take wd wd_num in
          let update = { withdrawn = wd; path_attrs = attrs; nlri = rest } in
          (update::partial, rest_wd)
        else (partial, wd)
      in
      
      (rest_wd, updates @ acc)
    in

    let wd, partial = List.fold_left aux_gen_updates (wd, []) db in

    let wd_updates =
      if wd <> [] then
        let aux, rest = split wd ((max_update_len - min_update_len) / pfx_len) in 
        let gen_wd_update pfxs = { withdrawn = pfxs; path_attrs = []; nlri = [] } in
        List.map gen_wd_update (rest::aux)
      else []
    in

    wd_updates @ partial
  ;;

  let process_updates t rev_updates =
    (* Construct the db of updates in reverse time order *)
    let wd_set, ins_set, ins_and_attrs = build_db rev_updates in
    
    let wd = if t.peer_group then begin
      let new_db, wd = 
        let tmp = Prefix_set.elements wd_set in
        let f (db, wd) p = 
          match Prefix_map.find_opt p db with
          | None -> (db, wd)
          | Some _ -> (Prefix_map.remove p db, p::wd)
        in
        List.fold_left f (t.db, []) tmp
      in
      t.db <- new_db;

      let new_db =
        let f _ (attrs, ins) acc =
          let g acc p = Prefix_map.add p attrs acc in
          List.fold_left g acc ins
        in
        ID_map.fold f ins_and_attrs t.db
      in
      t.db <- new_db; 

      wd
    end 
    else 
      Prefix_set.elements wd_set
    in

    let updates = gen_updates wd (ID_map.bindings ins_and_attrs) in

    (* Decision choice: must send all previous updates before handling next batch. *)
    let () = 
      let f _ cb = List.iter (fun u -> cb u) updates in
      Ip_map.iter f t.callbacks
    in
    
    t
  ;;

  let process_subs t (remote_id, callback) =
    (* Sync with new peer *)
    let sync_updates = 
      let pfx_and_attrs = Prefix_map.bindings t.db in
      let f (pfx, path_attrs) = { withdrawn = []; path_attrs; nlri = [pfx] } in
      List.map f pfx_and_attrs
    in
    let wd_set, ins_set, ins_and_attrs = build_db sync_updates in
    let updates = gen_updates [] (ID_map.bindings ins_and_attrs) in
    List.iter (fun u -> callback u) updates;

    t.callbacks <- Ip_map.add remote_id callback t.callbacks  
  ;;
    
  let rec handle_loop t = 
    let out_rib_handle inputs = 
      match List.mem Stop inputs with
      | true -> Lwt.return_unit
      | false ->
        let f = function
          | Sub (remote_id, callback) ->
            process_subs t (remote_id, callback)
          | _ -> ()
        in
        List.iter f inputs;

        let rev_updates = 
          let f acc = function
            | Push u -> u::acc
            | _ -> acc
          in
          List.fold_left f [] inputs
        in
        
        (* Filtering *)
        let f (wd, ins, attrs, attr_id) = 
          let updated_attrs = 
            if ins <> [] then
              if t.iBGP then  
                (* Remove MED *)
                let tmp = Bgp.set_med attrs None in
                (* TODO: Add LOCAL_PREF *)
                tmp
              else
                (* Remove LOCAL_PREF *)
                let tmp = Bgp.set_local_pref attrs None in
                (* Update NEXT HOP *)
                let tmp = Bgp.set_next_hop tmp t.local_id in
                (* Update AS PATH *)
                let tmp = prepend_aspath tmp t.local_asn in
                tmp
            else []
          in
          { withdrawn = wd; path_attrs = updated_attrs; nlri = ins }
        in
        let rev_updates = List.map f rev_updates in

        let t = process_updates t rev_updates in

        handle_loop t  
    in

    Lwt_stream.peek t.stream >>= fun input_opt ->
    match input_opt with
    | None -> Lwt.return_unit
    | Some _ -> 
      let inputs = Lwt_stream.get_available t.stream in
      out_rib_handle inputs
  ;;

  let sub t id callback = t.pf (Some (Sub (id, callback)))

  let unsub t id = 
    t.callbacks <- Ip_map.remove id t.callbacks;
    if Ip_map.cardinal t.callbacks = 0 then 
      t.db <- Prefix_map.empty
    else ()
  ;;

  let create id local_id local_asn iBGP transit filter_opt peer_group : t = 
    (* Initiate data structure *)
    let stream, pf = Lwt_stream.create () in
    let t = {
      id;
      local_id; local_asn; iBGP;
      callbacks = Ip_map.empty; 
      db = Prefix_map.empty;
      internal_transit = transit;
      stream; pf;
      filter_opt;
      peer_group;
    } in

    (* Spawn handling loop *)
    let _ = handle_loop t in

    t
  ;;
  
  let input t input = t.pf (Some input)

  let push_update t u = input t (Push u) 
  
  let stop t = input t Stop
end


module Loc_rib = struct
  type input = 
    | Push of Ipaddr.V4.t * (Adj_rib_in.output list)
    | Resolved of (Ipaddr.V4.Prefix.t * ((Ipaddr.V4.t * int) option)) list
    | Sub of Ipaddr.V4.t * int32 * Adj_rib_in.t * Adj_rib_out.t
    | Unsub of Ipaddr.V4.t * unit Lwt_condition.t
    | Stop

  type peer = {
    remote_id: Ipaddr.V4.t;
    remote_asn: int32;
    iBGP: bool;
    in_rib: Adj_rib_in.t;
    out_rib: Adj_rib_out.t;
  }

  type rte = {
    path_attrs: Bgp.path_attrs;
    attr_id: int;
    remote_id: Ipaddr.V4.t;
    remote_asn: int32;
    iBGP: bool;
    igp_metric: int;
    direct_gw: Ipaddr.V4.t;
    weight: int;
  }

  type t = {
    mutable running: bool;
    local_asn: int32;
    local_id: Ipaddr.V4.t;
    
    mutable subs: peer Ip_map.t;
    mutable db: int Prefix_map.t;
    mutable dict: rte Dict.t;
    mutable out_ribs: Adj_rib_out.t Dict.t;
    
    route_mgr: Route_mgr.t;
    hold_queue: (Ipaddr.V4.t * (Ipaddr.V4.Prefix.t list) * (Ipaddr.V4.Prefix.t list) * Bgp.path_attrs * int * int) Queue.t;
    
    stream: input Lwt_stream.t;
    pf: input option -> unit;
  }

  let t_ref : t option ref = ref None


  let get_aspath_len segments =
    let f_segment_len = function
      | Bgp.Asn_seq l -> List.length l
      | Bgp.Asn_set _ -> 1
    in
    let aux = List.map f_segment_len segments in
    let f_sum acc x = acc + x in
    List.fold_left f_sum 0 aux
  ;;

  (* Output true: 1st argument is more preferable, output false: 2nd argument is more preferable *)
  let is_better_route route1 route2 = 
    if route1.weight <> route2.weight then 
      (* Prefer route with higher weight *)
      route1.weight - route2.weight > 0
    else 
      let attrs1 = route1.path_attrs in
      let attrs2 = route2.path_attrs in
      let len_asp1 = get_aspath_len (Bgp.find_as_path attrs1) in
      let len_asp2 = get_aspath_len (Bgp.find_as_path attrs2) in
      if len_asp1 <> len_asp2 then 
        (* Prefer route with shorter AS PATH *)
        len_asp2 - len_asp1 > 0
      else
        let origin_1 = Bgp.origin_to_int (find_origin attrs1) in
        let origin_2 = Bgp.origin_to_int (find_origin attrs2) in
        if origin_1 <> origin_2 then
          (* Prefer route with lower origin number *)
          origin_2 - origin_1 > 0
        else
          if route1.remote_asn = route2.remote_asn then
            let med_1 = Bgp.find_med attrs1 in
            let med_2 = Bgp.find_med attrs2 in
            if med_1 = None || med_2 = None then
              (* Prefer eBGP over iBGP *)
              if route1.iBGP && not route2.iBGP then false
              else if (not route1.iBGP) && route2.iBGP then true
              else 
                if route1.igp_metric <> route2.igp_metric then 
                  (* Prefer route with lower IGP metric *)
                  route2.igp_metric - route1.igp_metric > 0
                else
                  (* Prefer route of smaller router id *)
                  Ipaddr.V4.compare route1.remote_id route2.remote_id < 0 
            else
              let v1 = option_get med_1 in
              let v2 = option_get med_2 in
              (* Prefer route with lower MED *)
              Int32.sub v2 v1 < 0_l
          else
            (* Prefer eBGP over iBGP *)
            if route1.iBGP && not route2.iBGP then false
            else if (not route1.iBGP) && route2.iBGP then true
            else 
              if route1.igp_metric <> route2.igp_metric then 
                (* Prefer route with lower IGP metric *)
                route2.igp_metric - route1.igp_metric > 0
              else
                (* Prefer route of smaller router id *)
                Ipaddr.V4.compare route1.remote_id route2.remote_id < 0 
  ;;

  
  (* This function is pure *)
  let update_loc_db (peer_id, wd, ins_and_resolved, path_attrs, attr_id, weight) local_asn (peers: peer Ip_map.t) db dict =
    let peer = Ip_map.find peer_id peers in
    
    (* Withdrawn from Loc-RIB *)
    let wd, db, dict =
      let loc_wd_pfx (wd, db, dict) pfx = 
        match Prefix_map.find_opt pfx db with
        | None -> wd, db, dict
        | Some rte_id -> 
          let rte = Dict.find rte_id dict in
          if (rte.remote_id = peer_id) then 
            (pfx::wd, Prefix_map.remove pfx db, Dict.remove rte_id dict) 
          else wd, db, dict
      in
      List.fold_left loc_wd_pfx ([], db, dict) wd
    in

    match List.find_opt (fun (_, x) -> x <> None) ins_and_resolved with
    | None ->
      let f (wd, db, dict) (pfx, _) =
        match Prefix_map.find_opt pfx db with
        | None -> (wd, db, dict) 
        | Some rte_id ->
          let rte = Dict.find rte_id dict in
          if rte.remote_id = peer_id then 
            (pfx::wd, Prefix_map.remove pfx db, Dict.remove rte_id dict) 
          else (wd, db, dict)
      in
      let (wd, db, dict) = List.fold_left f (wd, db, dict) ins_and_resolved in

      let rte = {
        path_attrs = path_attrs;
        attr_id = attr_id;
        weight;
        direct_gw = Ipaddr.V4.localhost;
        igp_metric = 0;
        remote_id = peer.remote_id;
        remote_asn = peer.remote_asn;
        iBGP = peer.iBGP;
      } in

      (wd, [], rte, db, dict)
    | Some (_, v) -> match v with
      | None -> Logs.err (fun m -> m "Really?! Come on!"); assert false
      | Some (direct_gw, igp_metric) -> begin
        

        let u_attrs, u_attr_id = 
          if peer.iBGP then (path_attrs, attr_id)  
          else
            let attrs = Bgp.set_local_pref path_attrs (Some (Int32.of_int weight)) in
            let id = ID.create attrs in
            (attrs, id)
        in        

        let rte = {
          path_attrs = u_attrs;
          attr_id = u_attr_id;
          weight;
          direct_gw;
          igp_metric;
          remote_id = peer.remote_id;
          remote_asn = peer.remote_asn;
          iBGP = peer.iBGP;
        } in
        let rte_id = ID.create rte in

        let wd, ins, db, dict =   
          let loc_ins_pfx (wd, ins, db, dict) (pfx, resolved) = 
            match List.mem pfx wd with
            | true -> 
              (* Do not install new attrs immediately after withdrawn *)
              (wd, ins, db, dict) 
            | false ->
              match resolved with
              | None -> begin
                (* Unreachable *)
                match Prefix_map.find_opt pfx db with
                | None -> (wd, ins, db, dict) 
                | Some rte_id ->
                  let rte = Dict.find rte_id dict in
                  if rte.remote_id = peer_id then 
                    (pfx::wd, ins, Prefix_map.remove pfx db, Dict.remove rte_id dict) 
                  else (wd, ins, db, dict)
              end
              | Some (direct_gw, igp_metric) ->            
                match Prefix_map.find_opt pfx db with
                | None -> 
                  (* If this is a new destination *)
                  (wd, pfx::ins, Prefix_map.add pfx rte_id db, Dict.add rte_id rte dict)
                | Some s_rte_id ->
                  let s_rte = Dict.find s_rte_id dict in
                  if is_better_route rte s_rte then
                    (* Replace if the new route is more preferable *)
                    let new_dict = Dict.add rte_id rte (Dict.remove s_rte_id dict) in
                    (wd, pfx::ins, Prefix_map.add pfx rte_id db, new_dict)
                  else if rte.remote_id = s_rte.remote_id then 
                    (* If from the same peer then the current best path is no longer valid. *) 
                    (* Remove the current best path and reselects the path later. *)
                    (wd, ins, Prefix_map.remove pfx db, Dict.remove s_rte_id dict)
                  else wd, ins, db, dict
            in
            List.fold_left loc_ins_pfx (wd, [], db, dict) ins_and_resolved
          in

          (wd, ins, rte, db, dict)
      end
  ;;

  let rec handle_loop t = 
    let loc_rib_handle = function
      | None -> 
        Lwt.return_unit
      | Some input ->
        match input with
        | Stop -> 
          (* Remove installed routes from kernel's routing table *)
          if Key_gen.kernel () && Key_gen.remove () then
            let f (pfx, _) = pfx in
            let l_rm = List.map f (Prefix_map.bindings t.db) in
            let open Route_mgr in
            let krt_change = { insert = []; remove = l_rm } in
            let () = Route_mgr.input t.route_mgr (Route_mgr.Krt_change krt_change) in
            Lwt.return_unit
          else Lwt.return_unit
        | Push (remote_id, changes) ->
          if not (Ip_map.mem remote_id t.subs) then handle_loop t
          else begin
            let callback l = t.pf (Some (Resolved l)) in
            let f (wd, ins, attrs, attr_id, weight) =
              Queue.push (remote_id, wd, ins, attrs, attr_id, weight) t.hold_queue;
              let nh = if ins <> [] then Bgp.find_next_hop attrs else Ipaddr.V4.localhost in
              Route_mgr.input t.route_mgr (Route_mgr.Resolve (ins, nh, callback))
            in
            List.iter f changes;
            handle_loop t
          end
        | Resolved (ins_and_resolved) ->
          let (remote_id, wd, _, attrs, attr_id, weight) = Queue.pop t.hold_queue in 
          if not (Ip_map.mem remote_id t.subs) then handle_loop t
          else begin
            let peer = Ip_map.find remote_id t.subs in

            let wd, ins, rte, new_db, new_dict = 
              update_loc_db (remote_id, wd, ins_and_resolved, attrs, attr_id, weight) t.local_asn t.subs t.db t.dict 
            in

            (* Send the updates: blocking *)
            let () = 
              if wd <> [] || ins <> [] then
                let update = (wd, ins, rte.path_attrs, rte.attr_id) in

                let open Adj_rib_out in
                let f _ out_rib =
                  if (out_rib.iBGP && peer.iBGP) || (peer.out_rib.id = out_rib.id && out_rib.internal_transit = false) then 
                    ()
                  else
                    Adj_rib_out.input out_rib (Adj_rib_out.Push update)
                in
                Dict.iter f t.out_ribs
              else ()
            in

            (* Pull routes: blocking *)
            let () = 
              if wd <> [] then
                let f _ peer =
                  Adj_rib_in.input peer.in_rib (Adj_rib_in.Pull wd)
                in
                Ip_map.iter f t.subs
              else ()
            in

            (* Update kernel's routing table *)
            let () = 
              if Key_gen.kernel () then begin
                if wd = [] && ins = [] then ()
                else 
                  let open Route_mgr in
                  let krt_change = 
                    let remove = wd in
                    let insert = 
                      List.map (fun p -> (p, rte.direct_gw)) ins
                    in
                    { insert; remove }
                  in
                  Route_mgr.input t.route_mgr (Route_mgr.Krt_change krt_change)
              end
              else ()
            in

            t.db <- new_db;
            t.dict <- new_dict;

            handle_loop t
          end
        | Sub (remote_id, remote_asn, in_rib, out_rib) ->
          if Ip_map.mem remote_id t.subs then begin
            Rib_log.err (fun m -> m "Duplicated rib subscription for remote %s" 
                                    (Ipaddr.V4.to_string remote_id));
            assert false
          end
          else begin
            let peer = { 
              remote_id; remote_asn; in_rib; out_rib;
              iBGP = remote_asn = t.local_asn;
            } in
            t.subs <- Ip_map.add remote_id peer t.subs;
            
            let () = 
              let open Adj_rib_out in
              let () = 
                match Dict.mem out_rib.id t.out_ribs with
                | false ->
                  (* Sync *)
                  let classify list = 
                    if list = [] then []
                    else
                      let sorted_by_val = List.sort (fun (x1, y1) (x2, y2) -> y1 - y2) list in
                      let rec lift (groups, curr_group, prev_v) = function
                        | [] -> (curr_group, prev_v)::groups
                        | (x, y)::tl ->
                          if y = prev_v then lift (groups, x::curr_group, prev_v) tl
                          else lift ((curr_group, prev_v)::groups, [x], y) tl
                      in
                      let _, prev_v = List.hd sorted_by_val in
                      lift ([], [], prev_v) sorted_by_val
                  in

                  let groups = classify (Prefix_map.bindings t.db) in

                  let f (ins, rte_id) =
                    let rte = Dict.find rte_id t.dict in
                    let update = ([], ins, rte.path_attrs, rte.attr_id) in
                    Adj_rib_out.push_update out_rib update
                  in
                  List.iter f groups 
                | true -> ()
              in
              t.out_ribs <- Dict.add out_rib.id out_rib t.out_ribs
            in

            handle_loop t
          end
        | Unsub (remote_id, cond_var) ->
          if not (Ip_map.mem remote_id t.subs) then begin
            Rib_log.err (fun m -> m "No subscription for remote %s" 
                                      (Ipaddr.V4.to_string remote_id));
            assert false
          end
          else begin
            (* Update subscribed ribs *)
            let peer = Ip_map.find remote_id t.subs in
            t.subs <- Ip_map.remove remote_id t.subs;

            let () =
              let open Adj_rib_out in
              t.out_ribs <- Dict.remove peer.out_rib.id t.out_ribs
            in

            (* Withdraw all previously advertised messages *)
            let wd = 
              let open Adj_rib_in in
              List.map (fun (pfx, _) -> pfx) (Prefix_map.bindings peer.in_rib.db) 
            in

            let wd, ins, rte, new_db, new_dict = 
              update_loc_db (remote_id, wd, [], [], 0, 0) t.local_asn t.subs t.db t.dict 
            in
            t.db <- new_db;
            t.dict <- new_dict;

            let () = 
              if Key_gen.kernel () then begin
                if wd = [] && ins = [] then ()
                else 
                  let open Route_mgr in
                  let krt_change = 
                    let remove = wd in
                    let insert = 
                      List.map (fun p -> (p, rte.direct_gw)) ins
                    in
                    { insert; remove }
                  in
                  Route_mgr.input t.route_mgr (Route_mgr.Krt_change krt_change)
              end
              else ()
            in

            let () = 
              if wd <> [] then
                let update = (wd, [], [], 0) in

                let open Adj_rib_out in
                let f _ out_rib =
                  if (out_rib.iBGP && peer.iBGP) || (peer.out_rib.id = out_rib.id && out_rib.internal_transit = false) then 
                    ()
                  else 
                    Adj_rib_out.input out_rib (Adj_rib_out.Push update)
                in
                Dict.iter f t.out_ribs
              else ()
            in
            
            (* Signal the waiting ROUTER thread to proceed *)
            Lwt_condition.signal cond_var ();

            handle_loop t
          end
    in

    t_ref := Some t;
    if not t.running then 
      Lwt.return_unit
    else
      Lwt_stream.get t.stream >>= fun input ->
      loc_rib_handle input
  ;;


  let create local_id local_asn route_mgr = 
    (* Initiate data structure *)
    let stream, pf = Lwt_stream.create () in
    let t = {
      running = true;
      local_id; local_asn;
      subs = Ip_map.empty;
      db = Prefix_map.empty;
      dict = Dict.empty;
      out_ribs = Dict.empty;
      route_mgr;
      hold_queue = Queue.create ();
      stream; pf;
    } in

    (* Spawn handling loop thread *)
    let _ = handle_loop t in

    t
  ;;
 
  let to_string t = 
    let count = ref 0 in
    let pfxs_str = 
      let f pfx rte_id acc = 
        let rte = Dict.find rte_id t.dict in
        count := !count + 1;
        let pfx_str = Printf.sprintf "%d: %s | %s | %s" 
                                      (!count)
                                      (Ipaddr.V4.Prefix.to_string pfx) 
                                      (Ipaddr.V4.to_string rte.remote_id) 
                                      (path_attrs_to_string rte.path_attrs) 
        in
        pfx_str::acc
      in 
      String.concat "\n" (List.rev (Prefix_map.fold f t.db []))
    in  
    Printf.sprintf "%s" pfxs_str
  ;;

  let size t = Prefix_map.cardinal t.db

  let input t input = t.pf (Some input)
  let push_update t (id, l) = input t (Push (id, l)) 
  
  let stop t = 
    input t Stop;
    t.running <- false;
    t_ref := None
  ;;

  let sub t (id, asn, in_rib, out_rib) = input t (Sub (id, asn, in_rib, out_rib))
  let unsub t id cvar = input t (Unsub (id, cvar)) 
end
