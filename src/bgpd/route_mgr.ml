open Lwt.Infix
open Route_injector
open Route_table

let mgr_log = Logs.Src.create ~doc:"Route Mgr Log" "Route Mgr"
module Mgr_log = (val Logs.src_log mgr_log : Logs.LOG)

type route = Route_injector.route

type krt_change = {
  insert: Ipaddr.V4.Prefix.t list * Ipaddr.V4.t;
  remove: Ipaddr.V4.Prefix.t list;
}

type callback = (Ipaddr.V4.Prefix.t * (Ipaddr.V4.t * int) option) list -> unit

type input = 
  | Krt_change of krt_change
  | Resolve of Ipaddr.V4.Prefix.t list * Ipaddr.V4.t * callback
  | Stop

module Prefix_set = Set.Make(Ipaddr.V4.Prefix)

type t = {
  table: Route_table.t;
  mutable cache: Prefix_set.t;
  stream: input Lwt_stream.t;
  pf: input option -> unit;
}

let rec handle_loop t =
  let route_mgr_handle = function
    | Resolve (pfxs, nh, cb) ->
      let f target_net =
        (target_net, Route_table.resolve_opt t.table target_net nh)
      in
      let result = List.map f pfxs in
      let () = cb result in
      handle_loop t
    | Krt_change change ->
      (* Remove *)
      let filtered = List.filter (fun p -> Prefix_set.mem p t.cache) change.remove in
      let rm_route acc pfx = Prefix_set.remove pfx acc in
      t.cache <- List.fold_left rm_route t.cache filtered;

      let () = 
        match Route_injector.Unix.del_routes filtered with
        | Result.Ok () -> ()
        | Result.Error _ -> 
          (* No real action is performed to handle the error *)
          Mgr_log.debug (fun m -> m " Error occurs when deleting route from kernel. ")
      in
      
      let l_ins, gw = change.insert in

      (* Add *)
      let need_to_rm = List.filter (fun p -> Prefix_set.mem p t.cache) l_ins in
      t.cache <- List.fold_left rm_route t.cache need_to_rm;

      let () = 
        match Route_injector.Unix.del_routes need_to_rm with
        | Result.Ok () -> ()
        | Result.Error _ -> 
          (* No real action is performed to handle the error *)
          Mgr_log.debug (fun m -> m " Error occurs when deleting route from kernel. ")
      in

      let add_route acc pfx = Prefix_set.add pfx acc in
      t.cache <- List.fold_left add_route t.cache l_ins;

      let () =
        match Route_injector.Unix.add_routes l_ins gw with
        | Result.Ok () -> ()
        | Result.Error _ -> 
          (* No real action is performed to handle the error *)
          Mgr_log.debug (fun m -> m " Error occurs when deleting route from kernel. ");
      in
      
      handle_loop t
    | Stop -> Lwt.return_unit
  in
  Lwt_stream.get t.stream >>= function
  | None -> 
    Mgr_log.err (fun m -> m "It is not recommended and potentially buggy to terminate in this way.");
    Lwt.return_unit
  | Some input -> route_mgr_handle input
;;

let create () =
  match Route_injector.Unix.get_routes () with
  | Result.Ok routes ->
    let f tbl route = 
      Route_table.add_static tbl route
    in
    let table = List.fold_left f Route_table.empty routes in
    let stream, pf = Lwt_stream.create () in
    let cache = Prefix_set.empty in
    let t = { table; stream; pf; cache; } in
    let _ = handle_loop t in
    t
  | Result.Error _ ->
    Logs.err (fun m -> m "Fail to start route_mgr");
    assert false
;;

let stop t = t.pf (Some Stop)

let input t v = t.pf (Some v)






