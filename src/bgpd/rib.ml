open Bgp
open Lwt.Infix

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


type update = Bgp.update
let is_empty_update u = u.withdrawn = [] && u.nlri = [] 


module Adj_rib_in = struct
  type input = 
    | Push of update
    | Pull of Prefix.t list
    | Stop
  
  
  type t = {
    mutable running: bool;
    remote_id: Ipaddr.V4.t;
    callback: update -> unit;
    db: Bgp.path_attrs Prefix_map.t;
    stream: input Lwt_stream.t;
    pf: input option -> unit;
  }

  let set_db new_db t = {
    running = t.running;
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
    if not t.running then Lwt.return_unit
    else
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
            let () = t.callback out_update in
            handle_loop new_t
        | Pull pfx_list -> begin
          let f pfx =
            match Prefix_map.find_opt pfx t.db with
            | None -> ()
            | Some path_attrs ->
              let update = {
                withdrawn = [];
                path_attrs;
                nlri = [ pfx ];
              } in
              t.callback update
          in
          let () = List.iter f pfx_list in
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
      running = true;
      remote_id; callback; db;
      stream; pf;
    } in
    
    (* Spawn event handle loop *)
    let _ = handle_loop t in

    t
  ;;

  let input t input = t.pf (Some input)
  
  let push_update t update = input t (Push update)
  
  let stop t = 
    input t Stop;
    t.running <- false
  ;;
end


module Adj_rib_out = struct
  module Prefix_set = Set.Make(Prefix)
  module Str_map = Map.Make(String)

  type input = 
    | Push of update
    | Stop

  type t = {
    mutable running: bool;
    remote_id: Ipaddr.V4.t;
    callback: update -> unit;
    past_routes: Prefix_set.t;
    stream: input Lwt_stream.t;
    pf: input option -> unit;
  }

  let set_past_routes pr t = {
    running = t.running;
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
    if not t.running then Lwt.return_unit
    else begin
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
          let () = List.iter (fun u -> t.callback u) updates in
          
          (* Decision choice: must send all previous updates before handling next batch. *)
          (* Update data structure *)
          let n_routes = Prefix_set.union (Prefix_set.diff t.past_routes wd_set) insert_set in
          let n_t = set_past_routes n_routes t in

          (* Minimal advertisement time interval 50ms *)
          OS.Time.sleep_ns (Duration.of_ms 0)
          >>= fun () ->

          handle_loop n_t
    end
  ;;


  let create remote_id callback : t = 
    (* Initiate data structure *)
    let stream, pf = Lwt_stream.create () in
    let past_routes = Prefix_set.empty in
    let t = {
      running = true;
      remote_id; callback; past_routes;
      stream; pf;
    } in

    (* Spawn handling loop *)
    let _ = handle_loop t in

    t
  ;;
  let input t input = t.pf (Some input)

  let push_update t u = input t (Push u) 
  let stop t = 
    input t Stop;
    t.running <- false
  ;;
end


module Loc_rib = struct
  
  module Ip_map = Map.Make(Ipaddr.V4)

  type input = 
    | Push of Ipaddr.V4.t * update
    | Sub of Ipaddr.V4.t * Adj_rib_in.t * Adj_rib_out.t
    | Unsub of Ipaddr.V4.t
    | Stop

  type peer = {
    remote_id: Ipaddr.V4.t;
    in_rib: Adj_rib_in.t;
    out_rib: Adj_rib_out.t;
  }

  type t = {
    mutable running: bool;
    local_asn: int32;
    local_id: Ipaddr.V4.t;
    subs: peer Ip_map.t;
    db: (Bgp.path_attrs * Ipaddr.V4.t) Prefix_map.t;
    stream: input Lwt_stream.t;
    pf: input option -> unit;
  }

  let set_subs subs t = {
    running = t.running;
    local_asn = t.local_asn;
    local_id = t.local_id;
    subs;
    db = t.db;
    stream = t.stream;
    pf = t.pf;
  }

  let set_db db t = {
    running = t.running;
    local_asn = t.local_asn;
    local_id = t.local_id;
    subs = t.subs;
    db;
    stream = t.stream;
    pf = t.pf;
  }

  let t_ref : t option ref = ref None
  
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
  let update_db local_id local_asn (peer_id, update) db =
    (* Withdrawn from Loc-RIB *)
    let (db_after_wd, out_wd) =
      let f (db, out_wd) pfx = 
        match Prefix_map.find_opt pfx db with
        | None -> db, out_wd
        | Some (_, stored) -> 
          if (stored = peer_id) then 
            (Prefix_map.remove pfx db, List.cons pfx out_wd) 
          else db, out_wd
      in
      List.fold_left f (db, []) update.withdrawn
    in

    (* If the advertised path is looping, don't install any new routes OR no advertised route *)
    if update.nlri = [] || is_aspath_loop local_asn (find_aspath update.path_attrs) then begin
      let out_update = { withdrawn = out_wd; path_attrs = []; nlri = [] } in
      (db_after_wd, out_update)
    end else
      let updated_path_attrs = 
        update.path_attrs 
        |> update_nexthop local_id 
        |> update_aspath local_asn
      in

      let db_after_insert, out_nlri = 
        let f (db, out_nlri) pfx = 
          match List.mem pfx out_wd with
          | true -> 
            (* We do not install new attrs immediately after withdrawn *)
            (db, out_nlri)
          | false ->
            match Prefix_map.find_opt pfx db with
            | None -> 
              (Prefix_map.add pfx (updated_path_attrs, peer_id) db, pfx::out_nlri)
            | Some (stored_pattrs, src_id) ->
              if src_id = peer_id then 
                (* If from the same peer, replace without compare *)
                (Prefix_map.add pfx (updated_path_attrs, peer_id) db, pfx::out_nlri)
              else if tie_break (updated_path_attrs, peer_id) (stored_pattrs, src_id) then
                (* Replace if the new route is more preferable *)
                (Prefix_map.add pfx (updated_path_attrs, peer_id) db, pfx::out_nlri)
              else db, out_nlri
        in
        List.fold_left f (db_after_wd, []) update.nlri
      in

      let out_update = {
        withdrawn = out_wd;
        path_attrs = updated_path_attrs;
        nlri = out_nlri
      } in 

      (db_after_insert, out_update)
  ;;

  let get_assoc_pfxes db remote_id = 
    let f acc (pfx, (_, stored_id)) =
      if remote_id = stored_id then pfx::acc else acc
    in
    List.fold_left f [] (Prefix_map.bindings db)
  ;;

  let rec handle_loop t = 
    t_ref := Some t;

    if not t.running then Lwt.return_unit
    else
      Lwt_stream.get t.stream >>= function
      | None -> Lwt.return_unit
      | Some input ->
        match input with
        | Stop -> Lwt.return_unit
        | Push (remote_id, update) ->
          if not (Ip_map.mem remote_id t.subs) then handle_loop t
          else begin
            let new_db, out_update = update_db t.local_id t.local_asn (remote_id, update) t.db in
            
            let ip_and_peer = Ip_map.bindings t.subs in

            (* Send the updates: blocking *)
            let () = 
              if not (is_empty_update out_update) then
                let f (peer_id, peer) = 
                  if peer_id <> remote_id then 
                    Adj_rib_out.input peer.out_rib (Adj_rib_out.Push out_update)
                in
                List.iter f ip_and_peer
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

            let new_t = set_db new_db t in
            handle_loop new_t
          end
        | Sub (remote_id, in_rib, out_rib) -> begin
          if Ip_map.mem remote_id t.subs then
            Rib_log.err (fun m -> m "Duplicated rib subscription for remote %s" 
                                    (Ipaddr.V4.to_string remote_id));
          
          let new_subs = Ip_map.add remote_id { remote_id; in_rib; out_rib } t.subs in
          let new_t = set_subs new_subs t in
            
          (* Synchronize with peer: blocking *)
          let () =
            let f (pfx, (attr, _)) = 
              let update = { withdrawn = []; path_attrs = attr; nlri = [pfx] } in
              Adj_rib_out.input out_rib (Adj_rib_out.Push update)
            in
            List.iter f (Prefix_map.bindings t.db)
          in

          handle_loop new_t
        end
        | Unsub remote_id ->
          if not (Ip_map.mem remote_id t.subs) then
            Rib_log.err (fun m -> m "No rib subscription for remote %s" 
                                      (Ipaddr.V4.to_string remote_id));

          (* Update subscribed ribs *)
          let new_subs = Ip_map.remove remote_id t.subs in

          let out_wd = get_assoc_pfxes t.db remote_id in

          (* Design choices: It is okay to use blocking operations as these operations should return almost immediately. *)
          match out_wd = [] with
          | true -> 
            let new_t = set_subs new_subs t in
            handle_loop new_t
          | false ->
            (* Generate update *)
            let out_update = { withdrawn = out_wd; path_attrs = []; nlri = [] } in

            (* Update db *)
            let new_db = 
              let f acc pfx = Prefix_map.remove pfx acc in
              List.fold_left f t.db out_wd
            in

            let new_t = t |> set_subs new_subs |> set_db new_db in
            let ip_and_peer = Ip_map.bindings new_t.subs in

            (* Send the updates: blocking *)
            let () =  
              let f (peer_id, peer) = 
                Adj_rib_out.input peer.out_rib (Adj_rib_out.Push out_update)
              in
              List.iter f ip_and_peer
            in

            (* Pull routes *)
            let () =
              let f (peer_id, peer) =
                Adj_rib_in.input peer.in_rib (Adj_rib_in.Pull out_update.withdrawn)
              in
              List.iter f ip_and_peer
            in

            handle_loop new_t
  ;;


  let create local_id local_asn = 
    (* Initiate data structure *)
    let stream, pf = Lwt_stream.create () in
    let t = {
      running = true;
      local_id; local_asn;
      subs = Ip_map.empty;
      db = Prefix_map.empty;
      stream; pf
    } in

    (* Spawn handling loop thread *)
    let _ = handle_loop t in

    t
  ;;
 
  let to_string t = 
    let count = ref 0 in
    let pfxs_str = 
      let f pfx (pa, id) acc = 
        count := !count + 1;
        let pfx_str = Printf.sprintf "%d: %s | %s | %s" 
                                      (!count)
                                      (Ipaddr.V4.Prefix.to_string pfx) 
                                      (Ipaddr.V4.to_string id) 
                                      (Bgp.path_attrs_to_string pa) 
        in
        pfx_str::acc
      in 
      String.concat "\n" (List.rev (Prefix_map.fold f t.db []))
    in  
    Printf.sprintf "Routes: \n %s" pfxs_str
  ;;

  let size t = Prefix_map.cardinal t.db

  let input t input = t.pf (Some input)
  let push_update t (id, u) = input t (Push (id, u)) 
  
  let stop t = 
    input t Stop;
    t.running <- false;
    t_ref := None
  ;;

  let sub t (id, in_rib, out_rib) = input t (Sub (id, in_rib, out_rib))
  let unsub t id = input t (Unsub id) 
end
