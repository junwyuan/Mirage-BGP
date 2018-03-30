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
  
  type t = {
    remote_id: Ipaddr.V4.t;
    callback: update -> unit;
    signal: unit -> unit;
    db: ID.t Prefix_map.t;
    dict: path_attrs Dict.t;
    stream: input Lwt_stream.t;
    pf: input option -> unit;
  }

  let set_store new_db new_dict t = {
    remote_id = t.remote_id;
    callback = t.callback;
    signal = t.signal;
    db = new_db; dict = new_dict;
    stream = t.stream;
    pf = t.pf;
  }
    
  let update_in_db { withdrawn; path_attrs; nlri } db dict = 
    let out_wd, db_aft_wd, dict_aft_wd =
      let f (wd, db, dict) pfx = 
        match Prefix_map.find_opt pfx db with
        | None -> (wd, db, dict)
        | Some attr_id -> (pfx::wd, Prefix_map.remove pfx db, Dict.remove attr_id dict)
      in
      List.fold_left f ([], db, dict) withdrawn
    in

    let attr_id = ID.create path_attrs in
    let db_aft_ins, dict_aft_ins = 
      let f (db, dict) pfx = 
        match Prefix_map.find_opt pfx db with
        | None ->
          (Prefix_map.add pfx attr_id db, Dict.add attr_id path_attrs dict)
        | Some stored ->
          let new_dict = Dict.add attr_id path_attrs dict |> Dict.remove stored in
          (Prefix_map.add pfx attr_id db, new_dict)
      in
      List.fold_left f (db_aft_wd, dict_aft_wd) nlri
    in

    let out_update = {
      withdrawn = out_wd;
      path_attrs;
      nlri;
    } in

    (db_aft_ins, dict_aft_ins, out_update)
  ;;

  let rec handle_loop t = 
    let in_rib_handle = function
      | None -> Lwt.return_unit
      | Some input -> match input with
        | Push update -> 
          let new_db, new_dict, out_update = update_in_db update t.db t.dict in
          let new_t = set_store new_db new_dict t in
          if is_empty_update out_update then 
            handle_loop new_t
          else 
            (* Handle the callback before handling next input request. This guarantees update message ordering *)
            let () = t.callback out_update in
            handle_loop new_t
        | Pull pfx_list -> 
          let f pfx =
            match Prefix_map.find_opt pfx t.db with
            | None -> ()
            | Some attr_id ->
              let path_attrs = Dict.find attr_id t.dict in
              let update = {
                withdrawn = [];
                path_attrs;
                nlri = [ pfx ];
              } in
              t.callback update
          in
          let () = List.iter f pfx_list in
          handle_loop t
        | Stop -> 
          (* Withdraw all previously advertised messages *)
          let wd = List.map (fun (pfx, _) -> pfx) (Prefix_map.bindings t.db) in
          let () = t.callback { withdrawn = wd; path_attrs = []; nlri = []; } in
          
          (* Free the Router thread *)
          let () = t.signal () in
          
          Lwt.return_unit
    in
    Lwt_stream.get t.stream >>= fun input ->
    in_rib_handle input
  ;;

  let create remote_id callback signal : t = 
    (* Construct the data structure *)
    let stream, pf = Lwt_stream.create () in
    let db = Prefix_map.empty in
    let dict = Dict.empty in
    let t = {
      remote_id; 
      callback; signal;
      db; dict;
      stream; pf;
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

  type input = 
    | Push of update
    | Stop

  type t = {
    (* States *)
    local_id: Ipaddr.V4.t;
    local_asn: int32;
    iBGP: bool;
    
    mutable callbacks: (update -> unit) Ip_map.t;
    mutable past_routes: Prefix_set.t;
    
    (* Queue *)
    stream: input Lwt_stream.t;
    pf: input option -> unit;
  }

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

  let gen_updates local_asn local_id iBGP wd db =
    let max_update_len = 4096 in
    let min_update_len = 23 in
    let pfx_len = 5 in
    
    (* Generate messages for each set of distinct path attributes *)
    let aux_gen_updates (wd, acc) (_id, (attrs, ins)) =
      (* Update attrs *)
      let updated_attrs = 
        if iBGP then  
          (* Remove MED *)
          let tmp = Bgp.set_med attrs None in
          (* TODO: Add LOCAL_PREF *)
          tmp
        else
          (* Remove LOCAL_PREF *)
          let tmp = Bgp.set_local_pref attrs None in
          (* Update NEXT HOP *)
          let tmp = Bgp.set_next_hop tmp local_id in
          (* Update AS PATH *)
          let tmp = prepend_aspath tmp local_asn in
          tmp
      in

      let attrs_len = len_path_attrs_buffer updated_attrs in
      let pfxs_list, rest = split ins ((max_update_len - min_update_len - attrs_len) / pfx_len) in
      let gen_ins_update ins = { withdrawn = []; path_attrs = updated_attrs; nlri = ins } in
      
      let updates, rest_wd = 
        let partial = List.map gen_ins_update pfxs_list in
        if rest <> [] then
          let wd_num = (max_update_len - min_update_len - attrs_len - pfx_len * (List.length rest)) / pfx_len in
          let wd, rest_wd = take wd wd_num in
          let update = { withdrawn = wd; path_attrs = updated_attrs; nlri = rest } in
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
    
  let rec handle_loop t = 
    let out_rib_handle = function
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
          let wd_set, ins_set, db = build_db rev_updates in
          let aux = Prefix_set.inter wd_set t.past_routes in
          let wd = Prefix_set.elements aux in
          let updates = gen_updates t.local_asn t.local_id t.iBGP wd (ID_map.bindings db) in
          
          (* Decision choice: must send all previous updates before handling next batch. *)
          let () = 
            let f _ cb = List.iter (fun u -> cb u) updates in
            Ip_map.iter f t.callbacks
          in

          (* Update data structure *)
          t.past_routes <- Prefix_set.union (Prefix_set.diff t.past_routes aux) ins_set;

          (* Minimal advertisement time interval *)
          (* OS.Time.sleep_ns (Duration.of_ms 50)
          >>= fun () -> *)

          handle_loop t
    in

    Lwt_stream.peek t.stream >>= fun input ->
    out_rib_handle input
  ;;


  let create local_id local_asn iBGP : t = 
    (* Initiate data structure *)
    let stream, pf = Lwt_stream.create () in
    let past_routes = Prefix_set.empty in
    let t = {
      local_id; local_asn; iBGP;
      callbacks = Ip_map.empty; 
      past_routes;
      stream; pf;
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
    | Push of Ipaddr.V4.t * update * ((Ipaddr.V4.Prefix.t * int) list)
    | Resolved of (Ipaddr.V4.Prefix.t * (Ipaddr.V4.t * int) option) list
    | Sub of Ipaddr.V4.t * int32 * Adj_rib_in.t * (update -> unit)
    | Unsub of Ipaddr.V4.t
    | Stop

  type peer = {
    remote_id: Ipaddr.V4.t;
    remote_asn: int32;
    iBGP: bool;
    in_rib: Adj_rib_in.t;
    cb: update -> unit;
  }

  type rte = {
    path_attrs: Bgp.path_attrs;
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
    
    route_mgr: Route_mgr.t;
    hold_queue: (Ipaddr.V4.t * update * ((Ipaddr.V4.Prefix.t * int) list)) Queue.t;
    
    stream: input Lwt_stream.t;
    pf: input option -> unit;

    ibgp_out_rib: Adj_rib_out.t;
    ebgp_out_rib: Adj_rib_out.t;
  }

  let t_ref : t option ref = ref None
  
  let is_aspath_loop local_asn segment_list =
    let f = function
      | Bgp.Asn_seq l -> List.mem local_asn l
      | Bgp.Asn_set l -> List.mem local_asn l
    in
    List.exists f segment_list
  ;;

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
  let update_loc_db (peer_id, update, weights, resolved_results) local_asn (peers: peer Ip_map.t) db dict =
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
      List.fold_left loc_wd_pfx ([], db, dict) update.withdrawn
    in

    (* If the advertised path is looping, don't install any new routes OR no advertised route *)
    if update.nlri = [] || is_aspath_loop local_asn (find_as_path update.path_attrs) then begin
      let out_update = { withdrawn = wd; path_attrs = []; nlri = [] } in
      (db, dict, out_update)
    end else
      let wd, ins, db, dict =   
        let loc_ins_pfx (wd, ins, db, dict) pfx = 
          match List.mem pfx wd with
          | true -> 
            (* Do not install new attrs immediately after withdrawn *)
            (wd, ins, db, dict) 
          | false ->
            match List.assoc pfx resolved_results with
            | None ->
              (* Unreachable *)
              (wd, ins, db, dict) 
            | Some (direct_gw, igp_metric) ->
              let weight = List.assoc pfx weights in
              let peer = Ip_map.find peer_id peers in
            
              (* let updated_attrs = 
                if peer.iBGP then 
                  update.path_attrs 
                else
                  Bgp.set_local_pref update.path_attrs (Some (Int32.of_int weight)) 
              in *)
              
              let rte = {
                path_attrs = update.path_attrs;
                weight;
                direct_gw;
                igp_metric;
                remote_id = peer.remote_id;
                remote_asn = peer.remote_asn;
                iBGP = peer.iBGP;
              } in
              let rte_id = ID.create rte in

              match Prefix_map.find_opt pfx db with
              | None -> 
                (* If this is a new destination *)
                (wd, pfx::ins, Prefix_map.add pfx rte_id db, Dict.add rte_id rte dict)
              | Some s_rte_id ->
                let s_rte = Dict.find s_rte_id dict in
                if is_better_route rte s_rte then
                  (* Replace if the new route is more preferable *)
                  (wd, pfx::ins, Prefix_map.add pfx rte_id db, Dict.add rte_id rte dict)
                else if rte.remote_id = s_rte.remote_id then 
                  (* If from the same peer then the current best path is no longer valid. *) 
                  (* Remove the current best path and reselects the path later. *)
                  (pfx::wd, ins, Prefix_map.remove pfx db, Dict.remove s_rte_id dict)
                else wd, ins, db, dict
        in
        List.fold_left loc_ins_pfx (wd, [], db, dict) update.nlri
      in

      let out_update = {
        withdrawn = wd;
        path_attrs = update.path_attrs;
        nlri = ins
      } in 

      (db, dict, out_update)
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
            let krt_change = { insert = [], Ipaddr.V4.localhost; remove = l_rm } in
            let () = Route_mgr.input t.route_mgr (Route_mgr.Krt_change krt_change) in
            Lwt.return_unit
          else Lwt.return_unit
        | Push (remote_id, update, weights) ->
          if (not (Ip_map.mem remote_id t.subs)) && update.nlri <> [] then handle_loop t
          else begin
            let callback result = 
              t.pf (Some (Resolved result)) 
            in

            Queue.push (remote_id, update, weights) t.hold_queue;
            if update.nlri = [] then begin
              (* Dummy request *)
              let next_hop = Ipaddr.V4.localhost in
              Route_mgr.input t.route_mgr (Route_mgr.Resolve ([], next_hop, callback));
              handle_loop t
            end
            else begin
              let next_hop = Bgp.find_next_hop update.path_attrs in
              Route_mgr.input t.route_mgr (Route_mgr.Resolve (update.nlri, next_hop, callback));
              handle_loop t
            end
          end
        | Resolved results ->
          let (remote_id, update, weights) = Queue.pop t.hold_queue in 
          if (not (Ip_map.mem remote_id t.subs)) && update.nlri <> [] then handle_loop t
          else begin
            let new_db, new_dict, out_update = 
              update_loc_db (remote_id, update, weights, results) t.local_asn t.subs t.db t.dict 
            in
              
            let ip_and_peer = Ip_map.bindings t.subs in

            (* Send the updates: blocking *)
            let () = 
              if not (is_empty_update out_update) then
                (* TODO distinguish iBGP and eBGP *)
                Adj_rib_out.input t.ebgp_out_rib (Adj_rib_out.Push out_update)
              else ()
            in

            (* Pull routes: blocking *)
            let () = 
              if out_update.withdrawn <> [] then
                let f (_, peer) =
                  Adj_rib_in.input peer.in_rib (Adj_rib_in.Pull out_update.withdrawn)
                in
                List.iter f ip_and_peer
              else ()
            in

            (* Update kernel's routing table *)
            let () = 
              if Key_gen.kernel () then begin
                let open Route_injector in
                let open Route_mgr in
                let krt_change = 
                  let remove = out_update.withdrawn in
                  let insert = 
                    if out_update.nlri <> [] then
                      let direct_gw = 
                        let _, opt = List.hd results in
                        let direct_gw, _metric = option_get opt in
                        direct_gw
                      in
                      out_update.nlri, direct_gw
                    else [], Ipaddr.V4.localhost
                  in
                  { insert; remove }
                in

                if out_update.nlri = [] && out_update.withdrawn = [] then ()
                else Route_mgr.input t.route_mgr (Route_mgr.Krt_change krt_change)
              end
              else ()
            in

            t.db <- new_db;
            t.dict <- new_dict;

            handle_loop t
          end
        | Sub (remote_id, remote_asn, in_rib, cb) ->
          if Ip_map.mem remote_id t.subs then begin
            Rib_log.err (fun m -> m "Duplicated rib subscription for remote %s" 
                                    (Ipaddr.V4.to_string remote_id));
            assert false
          end
          else begin
            let peer = { 
              remote_id; remote_asn; in_rib; cb;
              iBGP = remote_asn = t.local_asn;
            } in
            t.subs <- Ip_map.add remote_id peer t.subs;

            let () =
              let open Adj_rib_out in
              if peer.iBGP then 
                t.ibgp_out_rib.callbacks <- Ip_map.add remote_id cb t.ibgp_out_rib.callbacks
              else
                t.ebgp_out_rib.callbacks <- Ip_map.add remote_id cb t.ebgp_out_rib.callbacks
            in
            
            (* Synchronize with peer: blocking *)
            let () =
              let f (pfx, rte_id) = 
                let rte = Dict.find rte_id t.dict in
                let update = { withdrawn = []; path_attrs = rte.path_attrs; nlri = [ pfx ] } in
                cb update
              in
              List.iter f (Prefix_map.bindings t.db)
            in

            handle_loop t
          end
        | Unsub remote_id ->
          if not (Ip_map.mem remote_id t.subs) then begin
            Rib_log.err (fun m -> m "No rib subscription for remote %s" 
                                      (Ipaddr.V4.to_string remote_id));
            assert false
          end
          else begin
            (* Update subscribed ribs *)
            let peer = Ip_map.find remote_id t.subs in
            t.subs <- Ip_map.remove remote_id t.subs;
            let () = 
              let open Adj_rib_out in
              if peer.iBGP then 
                t.ibgp_out_rib.callbacks <- Ip_map.remove remote_id t.ibgp_out_rib.callbacks
              else
                t.ebgp_out_rib.callbacks <- Ip_map.remove remote_id t.ebgp_out_rib.callbacks
            in
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
      route_mgr;
      hold_queue = Queue.create ();
      stream; pf;

      ibgp_out_rib = Adj_rib_out.create local_id local_asn true;
      ebgp_out_rib = Adj_rib_out.create local_id local_asn false;
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
  let push_update t (id, u, w) = input t (Push (id, u, w)) 
  
  let stop t = 
    input t Stop;
    t.running <- false;
    t_ref := None
  ;;

  let sub t (id, asn, in_rib, out_rib) = input t (Sub (id, asn, in_rib, out_rib))
  let unsub t id = input t (Unsub id) 
end
