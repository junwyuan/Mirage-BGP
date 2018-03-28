open Lwt.Infix
open Route_injector
open Route_table

let mgr_log = Logs.Src.create ~doc:"Route Mgr Log" "Route Mgr"
module Mgr_log = (val Logs.src_log mgr_log : Logs.LOG)

type route = Route_injector.route

type krt_change = {
  insert: route list;
  remove: Ipaddr.V4.Prefix.t list;
}

type callback = ((Ipaddr.V4.Prefix.t * Ipaddr.V4.t) * (Ipaddr.V4.t * int) option) list -> unit

type input = 
  | Krt_change of krt_change
  | Resolve of (Ipaddr.V4.Prefix.t * Ipaddr.V4.t) list * callback
  | Stop

module Prefix_set = Set.Make(Ipaddr.V4.Prefix)

type t = {
  mutable table: Route_table.t;
  stream: input Lwt_stream.t;
  pf: input option -> unit;
}

let rec handle_loop t =
  let route_mgr_handle = function
    | Resolve (quests, cb) ->
      let f ((target_net, nh) as quest) =
        (quest, Route_table.resolve_opt t.table target_net nh)
      in
      let result = List.map f quests in
      let () = cb result in
      handle_loop t
    | Krt_change change ->
      (* Remove *)
      let f pfx = 
        match Route_injector.Unix.del_route pfx with
        | Result.Ok () -> ()
        | Result.Error _ -> 
          (* No real action is performed to handle the error *)
          Mgr_log.debug (fun m -> m " Error occurs when deleting route from kernel. ");
      in
      let () = List.iter f change.remove in

      (* Add *)
      let f route =
        let () = match Route_injector.Unix.del_route route.net with
          | Result.Ok () -> ()
          | Result.Error _ -> 
            (* No real action is performed to handle the error *)
            Mgr_log.debug (fun m -> m " Error occurs when deleting route from kernel. ")
        in
        let () = match Route_injector.Unix.add_route route.net (option_get route.gw) with
          | Result.Ok () -> ()
          | Result.Error _ -> 
            (* No real action is performed to handle the error *)
            Mgr_log.debug (fun m -> m " Error occurs when deleting route from kernel. ");
        in
        ()
      in
      let () = List.iter f change.insert in

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
    { table; stream; pf; }
  | Result.Error _ ->
    Logs.err (fun m -> m "fail to start route_mgr");
    assert false
;;

let stop t = t.pf (Some Stop)

let input t v = t.pf (Some v)







