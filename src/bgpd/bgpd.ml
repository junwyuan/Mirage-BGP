open Lwt.Infix

(* Design choices: Use signal-and-continue *)

(* Logging *)
let ctl_src = Logs.Src.create "Ctl" ~doc:"CTL logging"
module Ctl_log = (val Logs.src_log ctl_src : Logs.LOG)

module Id_map = Map.Make(Ipaddr.V4)

module  Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4) = struct
  module Router = Router.Make(S)
  open Router

  let rec command_loop console peers =
    C.read console >>= function
    | Error err -> 
      Ctl_log.warn (fun m -> m "Fail to read command: %a" C.pp_error err);
      Lwt.return_unit
    | Ok (`Eof) ->
      Ctl_log.warn (fun m -> m "Console closed?");
      Lwt.return_unit
    | Ok (`Data b) ->
      match String.trim @@ Cstruct.to_string b with
      | "start" -> 
        let f peer =
          Ctl_log.info (fun m -> m "BGP peer %s starts." (Ipaddr.V4.to_string peer.remote_id));
          push_event peer (Fsm.Manual_start)
        in
        let _ = Id_map.map f peers in
        command_loop console peers
      | "stop" -> 
        let f peer =
          Ctl_log.info (fun m -> m "BGP peer %s stops." (Ipaddr.V4.to_string peer.remote_id));
          push_event peer (Fsm.Manual_stop)
        in
        let _ = Id_map.map f peers in
        command_loop console peers
      | "show peers" ->
        let f peer =
          let fsm = Printf.sprintf "FSM: %s" (Fsm.to_string peer.fsm) in
          let running_dev =
            let dev1 = if not (peer.conn_retry_timer = None) then "Conn retry timer" else "" in
            let dev2 = if not (peer.hold_timer = None) then "Hold timer" else "" in 
            let dev3 = if not (peer.keepalive_timer = None) then "Keepalive timer" else "" in 
            let dev4 = if not (peer.conn_starter = None) then "Conn starter" else "" in 
            let dev5 = if not (peer.flow_handler = None) then "Flow handler" else "" in 
            let str_list = List.filter (fun x -> not (x = "")) [dev1; dev2; dev3; dev4; dev5] in
            Printf.sprintf "Running Dev: %s" (String.concat "; " str_list)
          in
          let msgs = 
            Printf.sprintf "Sent: %d OPEN; %d UPDATE; %d NOTIF; %d KEEPALIVE; \n Rec: %d OPEN; %d UPDATE; %d NOTIF; %d KEEPALIVE"
                          peer.stat.sent_open peer.stat.sent_update peer.stat.sent_notif peer.stat.sent_keepalive
                          peer.stat.rec_open peer.stat.rec_update peer.stat.rec_notif peer.stat.rec_keepalive
          in
          Ctl_log.info (fun m -> m "%s \n %s" (Ipaddr.V4.to_string peer.remote_id) (String.concat "\n" [fsm; running_dev; msgs]));
        in
        let _ = Id_map.map f peers in
        command_loop console peers
      | "show routes" ->
        let () = 
          match !Rib.Loc_rib.t_ref with
          | None -> Ctl_log.info (fun m -> m "Empty")
          | Some rib -> Ctl_log.info (fun m -> m "Routes:\n%s" (Rib.Loc_rib.to_string rib))
        in
        command_loop console peers
      | "show gc" ->
        let word_to_KB ws = ws * 8 / 1024 in

        let gc_stat = Gc.stat () in
        let open Gc in
        let allocation = Printf.sprintf "Minor: %.0f, Promoted: %.0f, Major %.0f" 
                                gc_stat.minor_words gc_stat.promoted_words gc_stat.major_words in
        let size = Printf.sprintf "Heap size: %d KB, Stack size: %d KB" 
                                (word_to_KB gc_stat.heap_words) (word_to_KB gc_stat.stack_size) in
        let collection = Printf.sprintf "Minor collection: %d, Major collection: %d, Compaction: %d" 
                                gc_stat.minor_collections gc_stat.major_collections gc_stat.compactions in
        Ctl_log.info (fun m -> m "%s" (String.concat "\n" ["GC stat:"; allocation; size; collection]));
        
        command_loop console peers
      | "exit" ->
        Lwt.return_unit
      | str -> 
        Ctl_log.info (fun m -> m "Unrecognised command %s" str);
        command_loop console peers
  ;;

  (* let parse_config kv =
    let key = Key_gen.config () in
    KV.size kv key >>= function
    | Error e -> 
      Ctl_log.err (fun f -> f "Could not read config file: %a" KV.pp_error e);
      Lwt.return Config_parser.default_config
    | Ok size -> 
      KV.read kv key 0L size >>= function
      | Error e ->
        Ctl_log.err (fun f -> f "Could not read config file: %a" KV.pp_error e);
        Lwt.return Config_parser.default_config
      | Ok data ->
        let str = String.concat "" @@ List.map (fun b -> Cstruct.to_string b) data in     
        Lwt.return @@ Config_parser.parse_from_string str
  ;; *)

  let start console s =
    let open Config_parser in

    (* Record backtrace *)
    Printexc.record_backtrace true;

    (* Parse config from file *)
    (* parse_config kv >>= fun config -> *)
    let config = parse_from_file (Key_gen.config ()) in

    let route_mgr = Route_mgr.create () in

    (* Init loc-rib *)
    let loc_rib = Rib.Loc_rib.create config.local_id config.local_asn route_mgr in


    let c = ref 0 in

    (* Init shared data *)
    let init_t peer = 
      let out_rib = Rib.Adj_rib_out.create !c config.local_id config.local_asn (config.local_asn = peer.remote_asn) in
      c := !c + 1;
      Router.create s loc_rib out_rib config peer 
    in
    let t_list = List.map init_t config.peers in
    let peers = List.fold_left (fun acc (peer: Router.t) -> Id_map.add peer.remote_id peer acc) (Id_map.empty) t_list in

    (* Start listening to BGP port. *)
    let local_port = config.local_port in
    Router.listen_tcp_connection s local_port peers;
    
    (* Automatic passive start *)
    let f _id (peer: Router.t) =
      Ctl_log.info (fun m -> m "BGP peer %s autostarts." (Ipaddr.V4.to_string peer.remote_id));
      push_event peer Fsm.Automatic_start_passive_tcp
    in
    let () = Id_map.iter f peers in


    (if Key_gen.test () then 
      OS.Time.sleep_ns (Duration.of_sec (Key_gen.runtime ()))
    else 
      command_loop console peers
    )
    >>= fun () ->

    (* Clean up *)
    let f _id (peer: Router.t) = Router.stop peer in
    let () = Id_map.iter f peers in
    S.disconnect s
  ;;
end
