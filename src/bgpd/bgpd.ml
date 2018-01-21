open Lwt.Infix
open Bgp

(* Logging *)
let bgpd_src = Logs.Src.create "BGP" ~doc:"BGP logging"
module Bgp_log = (val Logs.src_log bgpd_src : Logs.LOG)

let peer_tag : Ipaddr.V4.t Logs.Tag.def =
  Logs.Tag.def "peer" ~doc:"Peer id" (fun ppf id -> Format.fprintf ppf "%s" (Ipaddr.V4.to_string id))

let stamp id = Logs.Tag.(empty |> add peer_tag id)

(* This is to simulate a cancellable lwt thread *)
module Device = struct
  type t = unit Lwt.t

  let create task callback : t =
    let t, u = Lwt.task () in
    let _ = 
      task () >>= fun x ->
      Lwt.wakeup u x;
      Lwt.return_unit
    in
    t >>= fun x ->
    (* To avoid callback being cancelled. *)
    let _ = callback x in
    Lwt.return_unit
  ;;

  let stop t = Lwt.cancel t
end

module  Main (S: Mirage_stack_lwt.V4) = struct
  module Bgp_flow = Bgp_io.Make(S)
  module Id_map = Map.Make(Ipaddr.V4)

  type stat = {
    mutable sent_open: int;
    mutable sent_update: int;
    mutable sent_keepalive: int;
    mutable sent_notif: int;
    mutable rec_open: int;
    mutable rec_update: int;
    mutable rec_keepalive: int;
    mutable rec_notif: int;
  }
  
  type t = {
    remote_id: Ipaddr.V4.t;
    remote_port: int;
    remote_asn: int32;
    local_id: Ipaddr.V4.t;
    local_port: int;
    local_asn: int32;
    socket: S.t;
    mutable fsm: Fsm.t;
    mutable flow: Bgp_flow.t option;
    mutable conn_retry_timer: Device.t option;
    mutable hold_timer: Device.t option;
    mutable keepalive_timer: Device.t option;
    mutable conn_starter: Device.t option;
    mutable flow_reader: Device.t option;
    mutable input_rib: Rib.Adj_rib_in.t option;
    mutable output_rib: Rib.Adj_rib_out.t option;
    mutable loc_rib: Rib.Loc_rib.t;
    stat: stat;
  }

  type id_map = t Id_map.t

  let create_timer time callback : Device.t =
    Device.create (fun () -> OS.Time.sleep_ns (Duration.of_sec time)) callback
  ;;

  let rec flow_reader t callback =
    match t.flow with
    | None -> Lwt.return_unit
    | Some flow -> begin
      let task () = Bgp_flow.read flow in
      let wrapped_callback read_result =
        match read_result with
        | Ok msg -> 
          let event = 
            match msg with
            | Bgp.Open o -> 
              t.stat.rec_open <- t.stat.rec_open + 1; 
              (* Open message err checking *)
              if o.version <> 4 then Fsm.Bgp_open_msg_err (Unsupported_version_number 4)
              (* This is not exactly what the specification indicates *)
              else if o.local_asn <> t.remote_asn then Fsm.Bgp_open_msg_err Bad_peer_as
              else Fsm.BGP_open o
            | Bgp.Update u -> begin
              t.stat.rec_update <- t.stat.rec_update + 1;

              match find_aspath u.path_attrs with
              | None -> Fsm.Update_msg_err (Missing_wellknown_attribute 2)
              | Some [] -> Fsm.Update_msg_err Malformed_as_path
              | Some (hd::tl) ->
                match hd with
                | Asn_seq l -> 
                  if List.hd l <> t.remote_asn then Fsm.Update_msg_err Malformed_as_path
                  else Fsm.Update_msg u
                | Asn_set l ->
                  if List.mem t.remote_asn l then Fsm.Update_msg_err Malformed_as_path
                  else Fsm.Update_msg u
                  
            end
            | Bgp.Notification e -> 
              t.stat.rec_notif <- t.stat.rec_notif + 1; 
              Fsm.Notif_msg e
            | Bgp.Keepalive -> 
              t.stat.rec_keepalive <- t.stat.rec_keepalive + 1; 
              Fsm.Keepalive_msg
          in
          Bgp_log.debug (fun m -> m "receive message %s" (Bgp.to_string msg) ~tags:(stamp t.remote_id));
          let _ = callback event in
          flow_reader t callback
        | Error err ->
          t.flow_reader <- None;
          (match err with
          | `Closed -> 
            Bgp_log.debug (fun m -> m "Connection closed when read." ~tags:(stamp t.remote_id));
            callback Fsm.Tcp_connection_fail
          | `Refused -> 
            Bgp_log.debug (fun m -> m "Read refused." ~tags:(stamp t.remote_id));
            callback Fsm.Tcp_connection_fail
          | `Timeout -> 
            Bgp_log.debug (fun m -> m "Read timeout." ~tags:(stamp t.remote_id));
            callback Fsm.Tcp_connection_fail
          | `PARSE_ERROR err -> begin
            match err with
            | Bgp.Parsing_error -> 
              Bgp_log.warn (fun m -> m "Message parsing error" ~tags:(stamp t.remote_id));
              (* I don't know what the correct event for this should be. *)
              callback Fsm.Tcp_connection_fail
            | Bgp.Msg_fmt_error err -> begin
              Bgp_log.warn (fun m -> m "Message format error" ~tags:(stamp t.remote_id));
              match err with
              | Bgp.Parse_msg_h_err sub_err -> callback (Fsm.Bgp_header_err sub_err)
              | Bgp.Parse_open_msg_err sub_err -> callback (Fsm.Bgp_open_msg_err sub_err)
              | Bgp.Parse_update_msg_err sub_err -> callback (Fsm.Update_msg_err sub_err)
            end
            | Bgp.Notif_fmt_error _ -> 
              Bgp_log.err (fun m -> m "Got an notification message error" ~tags:(stamp t.remote_id));
              (* I don't know what the correct event for this should be. *)
              (* I should log this event locally *)
              callback Fsm.Tcp_connection_fail
          end
          | _ -> 
            Bgp_log.warn (fun m -> m "Unknown read error in flow reader" ~tags:(stamp t.remote_id));
            Lwt.return_unit
          )
      in
      t.flow_reader <- Some (Device.create task wrapped_callback);
      Lwt.return_unit
    end
  ;;

  let start_flow_reader t callback = 
    match t.flow_reader with
    | None -> (match t.flow with
      | None -> 
        Bgp_log.warn (fun m -> m "new flow reader is created when no tcp flow." ~tags:(stamp t.remote_id)); 
        Lwt.return_unit
      | Some _ -> flow_reader t callback)
    | Some _ -> 
      Bgp_log.warn (fun m -> m "new flow reader is created when thee exists another flow reader." ~tags:(stamp t.remote_id)); 
      Lwt.return_unit
  ;;

  let init_tcp_connection t callback =
    Bgp_log.info (fun m -> m "try setting up TCP connection with remote" ~tags:(stamp t.remote_id));
    if not (t.conn_starter = None) then begin
      Bgp_log.warn (fun m -> m "new connection is initiated when there exists another conn starter." ~tags:(stamp t.remote_id));
      Lwt.return_unit
    end
    else if not (t.flow = None) then begin
      Bgp_log.warn (fun m -> m "new connection is initiated when there exists an old connection." ~tags:(stamp t.remote_id));
      Lwt.return_unit
    end
    else begin
      let task = fun () ->
        Bgp_flow.create_connection t.socket (t.remote_id, t.remote_port)
      in
      let wrapped_callback result =
        t.conn_starter <- None;
        match result with
        | Error err ->
          (match err with
          | `Timeout -> Bgp_log.info (fun m -> m "Connection init timeout." ~tags:(stamp t.remote_id))
          | `Refused -> Bgp_log.info (fun m -> m "Connection init refused." ~tags:(stamp t.remote_id))
          | _ -> Bgp_log.info (fun m -> m "Unknown connection init error." ~tags:(stamp t.remote_id)));
          
          callback (Fsm.Tcp_connection_fail)
        | Ok flow -> begin
          Bgp_log.info (fun m -> m "Connected to remote %s" (Ipaddr.V4.to_string t.remote_id));
          let connection = t in
          let open Fsm in
          match connection.fsm.state with
          | IDLE -> 
            Bgp_log.info (fun m -> m "Drop connection to remote %s because fsm is at IDLE" (Ipaddr.V4.to_string t.remote_id));
            Bgp_flow.close flow
          | CONNECT | ACTIVE ->
            connection.flow <- Some flow;
            callback Tcp_CR_Acked
            >>= fun () ->
            flow_reader connection callback
          | OPEN_SENT | OPEN_CONFIRMED -> 
            if (Ipaddr.V4.compare connection.local_id connection.remote_id < 0) then begin
              Bgp_log.info (fun m -> m "Connection collision detected and dump new connection." ~tags:(stamp t.remote_id));
              Bgp_flow.close flow
            end
            else begin
              Bgp_log.info (fun m -> m "Connection collision detected and dump existing connection." ~tags:(stamp t.remote_id));
              callback Open_collision_dump
              >>= fun () ->
              let new_fsm = {
                state = CONNECT;
                conn_retry_counter = connection.fsm.conn_retry_counter;
                conn_retry_time = connection.fsm.conn_retry_time;
                hold_time = connection.fsm.hold_time;
                keepalive_time = connection.fsm.keepalive_time;
              } in
              connection.flow <- Some flow;
              connection.fsm <- new_fsm;
              callback Tcp_CR_Acked
            end
          | ESTABLISHED -> 
            Bgp_log.info (fun m -> m "Connection collision detected and dump new connection.");
            Bgp_flow.close flow
        end
      in
      t.conn_starter <- Some (Device.create task wrapped_callback);
      Lwt.return_unit
    end
  ;;      

  let listen_tcp_connection s local_port id_map callback =
    let on_connect flow =
      let remote_id, _ = Bgp_flow.dst flow in
      Bgp_log.info (fun m -> m "receive incoming connection from remote %s" (Ipaddr.V4.to_string remote_id));
      match Id_map.mem remote_id id_map with
      | false -> 
        Bgp_log.info (fun m -> m "Refuse connection because remote id %s is unknown." (Ipaddr.V4.to_string remote_id));
        Bgp_flow.close flow
      | true -> begin
        let connection = Id_map.find remote_id id_map in
        let open Fsm in
        match connection.fsm.state with
        | IDLE -> 
          Bgp_log.info (fun m -> m "Refuse connection %s because fsm is at IDLE." (Ipaddr.V4.to_string remote_id));
          Bgp_flow.close flow
        | CONNECT | ACTIVE ->
          connection.flow <- Some flow;
          callback connection Tcp_connection_confirmed
          >>= fun () ->
          flow_reader connection (callback connection)
        | OPEN_SENT | OPEN_CONFIRMED -> 
          if (Ipaddr.V4.compare connection.local_id connection.remote_id > 0) then begin
            Bgp_log.info (fun m -> m "Collision detected and dump new connection.");
            Bgp_flow.close flow
          end
          else begin
            Bgp_log.info (fun m -> m "Collision detected and dump existing connection.");
            callback connection Open_collision_dump
            >>= fun () ->
            let new_fsm = {
              state = CONNECT;
              conn_retry_counter = connection.fsm.conn_retry_counter;
              conn_retry_time = connection.fsm.conn_retry_time;
              hold_time = connection.fsm.hold_time;
              keepalive_time = connection.fsm.keepalive_time;
            } in
            connection.flow <- Some flow;
            connection.fsm <- new_fsm;
            callback connection Tcp_connection_confirmed
          end
        | ESTABLISHED -> 
          Bgp_log.info (fun m -> m "Connection collision detected and dump new connection.");
          Bgp_flow.close flow
      end
    in
    Bgp_flow.listen s local_port on_connect
  ;;
                    
  let send_msg t msg = 
    match t.flow with
    | Some flow ->
      Bgp_log.debug (fun m -> m "send message %s" (Bgp.to_string msg) ~tags:(stamp t.remote_id));
      (match msg with
      | Bgp.Open _ -> t.stat.sent_open <- t.stat.sent_open + 1;
      | Bgp.Update _ -> t.stat.sent_update <- t.stat.sent_update + 1;
      | Bgp.Notification _ -> t.stat.sent_notif <- t.stat.sent_notif + 1;
      | Bgp.Keepalive -> t.stat.sent_keepalive <- t.stat.sent_keepalive + 1;
      );
      Bgp_flow.write flow msg
      >>= begin function
      | Error err ->
        (match err with
        | `Timeout -> Bgp_log.debug (fun m -> m "Timeout when write %s" (Bgp.to_string msg) ~tags:(stamp t.remote_id))
        | `Refused -> Bgp_log.debug (fun m -> m "Refused when Write %s" (Bgp.to_string msg) ~tags:(stamp t.remote_id))
        | `Closed -> Bgp_log.debug (fun m -> m "Connection closed when write %s." (Bgp.to_string msg) ~tags:(stamp t.remote_id)) 
        | _ -> ());
        Lwt.return_unit
      | Ok () -> Lwt.return_unit
      end    
    | None -> Lwt.return_unit
  ;;

  let send_open_msg (t: t) =
    let open Bgp in
    let open Fsm in
    let o = {
      version = 4;
      local_asn = t.local_asn;
      hold_time = t.fsm.hold_time;
      local_id = t.local_id;
      options = [];
    } in
    send_msg t (Bgp.Open o) 
  ;;

  let drop_tcp_connection t =    
    (match t.conn_starter with
    | None -> ()
    | Some d -> 
      Bgp_log.debug (fun m -> m "close conn starter." ~tags:(stamp t.remote_id)); 
      t.conn_starter <- None;
      Device.stop d);

    (match t.flow_reader with
    | None -> ()
    | Some d -> 
      Bgp_log.debug  (fun m -> m "close flow reader." ~tags:(stamp t.remote_id)); 
      t.flow_reader <- None;
      Device.stop d);
    
    match t.flow with
    | None -> Lwt.return_unit
    | Some flow ->
      Bgp_log.debug  (fun m -> m "close flow." ~tags:(stamp t.remote_id)); 
      t.flow <- None; 
      Bgp_flow.close flow;
  ;;


  let rec perform_action t action =
    let open Fsm in
    let callback = fun event -> handle_event t event in
    match action with
    | Initiate_tcp_connection -> init_tcp_connection t callback
    | Send_open_msg -> send_open_msg t
    | Send_msg msg -> send_msg t msg
    | Drop_tcp_connection -> drop_tcp_connection t
    | Start_conn_retry_timer -> 
      if (t.fsm.conn_retry_time > 0) then begin
        let callback () =
          t.conn_retry_timer <- None;
          handle_event t Connection_retry_timer_expired
        in
        t.conn_retry_timer <- Some (create_timer t.fsm.conn_retry_time callback);
      end;
      Lwt.return_unit
    | Stop_conn_retry_timer -> begin
      match t.conn_retry_timer with
      | None -> Lwt.return_unit
      | Some d -> t.conn_retry_timer <- None; Device.stop d; Lwt.return_unit
    end
    | Reset_conn_retry_timer -> begin
      (match t.conn_retry_timer with
      | None -> ()
      | Some t -> Device.stop t);
      if (t.fsm.conn_retry_time > 0) then begin
        let callback () =
          t.conn_retry_timer <- None;
          handle_event t Connection_retry_timer_expired
        in
        t.conn_retry_timer <- Some (create_timer t.fsm.conn_retry_time callback);
      end;
      Lwt.return_unit
    end
    | Start_hold_timer ht -> 
      if (t.fsm.hold_time > 0) then 
        t.hold_timer <- Some (create_timer ht (fun () -> t.hold_timer <- None; handle_event t Hold_timer_expired));
      Lwt.return_unit
    | Stop_hold_timer -> begin
      match t.hold_timer with
      | None -> Lwt.return_unit
      | Some d -> t.hold_timer <- None; Device.stop d; Lwt.return_unit
    end
    | Reset_hold_timer ht -> begin
      (match t.hold_timer with
      | None -> ()
      | Some d -> Device.stop d);
      if (t.fsm.hold_time > 0) then 
        t.hold_timer <- Some (create_timer ht (fun () -> t.hold_timer <- None; handle_event t Hold_timer_expired));
      Lwt.return_unit
    end
    | Start_keepalive_timer -> 
      if (t.fsm.keepalive_time > 0) then 
        t.keepalive_timer <- Some (create_timer t.fsm.keepalive_time 
                            (fun () -> t.keepalive_timer <- None; handle_event t Keepalive_timer_expired));
      Lwt.return_unit
    | Stop_keepalive_timer -> begin
      match t.keepalive_timer with
      | None -> Lwt.return_unit
      | Some d -> t.keepalive_timer <- None; Device.stop d; Lwt.return_unit
    end
    | Reset_keepalive_timer -> begin
      (match t.keepalive_timer with
      | None -> ()
      | Some t -> Device.stop t);
      if (t.fsm.keepalive_time > 0) then 
        t.keepalive_timer <- Some (create_timer t.fsm.keepalive_time 
                            (fun () -> t.keepalive_timer <- None; handle_event t Keepalive_timer_expired));
      Lwt.return_unit
    end
    | Process_update_msg u -> begin
      match t.input_rib with
      | None -> Bgp_log.err (fun m -> m "Input RIB not initiated"); Lwt.fail_with "Input RIB not initiated."
      | Some rib -> Rib.Adj_rib_in.handle_update rib u
    end
    | Initiate_rib ->
      let input_rib = 
        let callback u = 
          Rib.Loc_rib.handle_signal t.loc_rib (Rib.Loc_rib.Update (u, t.remote_id)) 
        in
        Rib.Adj_rib_in.create t.remote_id callback
      in
      t.input_rib <- Some input_rib;

      let output_rib =
        let callback u = 
          send_msg t (Bgp.Update u)
        in
        Rib.Adj_rib_out.create t.remote_id callback
      in
      t.output_rib <- Some output_rib;

      Rib.Loc_rib.handle_signal t.loc_rib (Rib.Loc_rib.Subscribe output_rib)
    | Release_rib ->
      t.input_rib <- None;
      match t.output_rib with
      | None -> Lwt.return_unit
      | Some rib -> 
        t.output_rib <- None; 
        Rib.Adj_rib_out.close rib;
        Rib.Loc_rib.handle_signal t.loc_rib (Rib.Loc_rib.Unsubscribe rib)
  
  and handle_event t event =
    let new_fsm, actions = Fsm.handle t.fsm event in
    
    (* Update finite state machine before performing any action. *)
    t.fsm <- new_fsm;
    
    (* Spawn threads to perform actions from left to right *)
    let rec perform_actions = function
      | [] -> Lwt.return_unit
      | hd::tl ->
        let handle exn =
          let raw_trace = Printexc.get_backtrace () in
          Bgp_log.err (fun m -> m "Some exception has occured: %s \n %s" (Printexc.to_string exn)raw_trace);
          (* Lwt.fail exn  *)
          Lwt.return_unit
        in
        Lwt.catch (fun () -> perform_action t hd) handle
        >>= fun () ->
        perform_actions tl
    in
    let _ = perform_actions actions in
    
    match Fsm.state t.fsm with
    | Fsm.IDLE -> begin
      match event with
      | Fsm.Manual_stop -> Lwt.return_unit
      | _ ->
        (* Automatic restart *)
        Bgp_log.info (fun m -> m "BGP automatic restarts." ~tags:(stamp t.remote_id));
        handle_event t Fsm.Automatic_start_passive_tcp
    end
    | _ -> Lwt.return_unit
  ;;

  let rec command_loop id_map =
    Lwt_io.read_line Lwt_io.stdin
    >>= function
    | "start" -> 
      let f t =
        Bgp_log.info (fun m -> m "BGP starts." ~tags:(stamp t.remote_id));
        handle_event t (Fsm.Manual_start)
      in
      let _ = Id_map.map f id_map in
      command_loop id_map
    | "stop" -> 
      let f t =
        Bgp_log.info (fun m -> m "BGP stops." ~tags:(stamp t.remote_id));
        handle_event t (Fsm.Manual_stop)
      in
      let _ = Id_map.map f id_map in
      command_loop id_map
    | "show peers" ->
      let f t =
        let fsm = Printf.sprintf "FSM: %s" (Fsm.to_string t.fsm) in
        let running_dev =
          let dev1 = if not (t.conn_retry_timer = None) then "Conn retry timer" else "" in
          let dev2 = if not (t.hold_timer = None) then "Hold timer" else "" in 
          let dev3 = if not (t.keepalive_timer = None) then "Keepalive timer" else "" in 
          let dev4 = if not (t.conn_starter = None) then "Conn starter" else "" in 
          let dev5 = if not (t.flow_reader = None) then "Flow reader" else "" in 
          let str_list = List.filter (fun x -> not (x = "")) [dev1; dev2; dev3; dev4; dev5] in
          Printf.sprintf "Running Dev: %s" (String.concat "; " str_list)
        in
        let rib =
          let input = 
          match t.input_rib with
            | None -> "No IN RIB."
            | Some rib -> Printf.sprintf "Adj_RIB_IN %d" (Rib.Adj_rib_in.size rib)
          in

          let loc = Printf.sprintf "Loc_RIB %d" (Rib.Loc_rib.size t.loc_rib) in
            
          let output = 
            match t.output_rib with
            | None -> "No OUT RIB"
            | Some rib -> Printf.sprintf "Adj_RIB_OUT %d" (Rib.Adj_rib_out.size rib)
          in
        
          (String.concat "; " [input; loc; output])
        in
        let msgs = 
          Printf.sprintf "Sent: %d OPEN; %d UPDATE; %d NOTIF; %d KEEPALIVE; \n Rec: %d OPEN; %d UPDATE; %d NOTIF; %d KEEPALIVE"
                        t.stat.sent_open t.stat.sent_update t.stat.sent_notif t.stat.sent_keepalive
                        t.stat.rec_open t.stat.rec_update t.stat.rec_notif t.stat.rec_keepalive
        in
        Bgp_log.info (fun m -> m "%s \n %s" (Ipaddr.V4.to_string t.remote_id) (String.concat "\n" [fsm; running_dev; rib; msgs]));
      in
      let _ = Id_map.map f id_map in
      command_loop id_map
    (* | "show rib detail" ->
      let input = 
        match t.input_rib with
        | None -> "No IN RIB."
        | Some rib -> Printf.sprintf "Adj_RIB_IN \n %s" (Rib.Adj_rib.to_string rib)
      in

      let loc = Printf.sprintf "%s" (Rib.Loc_rib.to_string t.loc_rib) in

      let output = 
        match t.output_rib with
        | None -> "No OUT RIB."
        | Some rib -> Printf.sprintf "Adj_RIB_OUT \n %s" (Rib.Adj_rib.to_string rib)
      in

      Bgp_log.info (fun m -> m "%s" (String.concat "\n" [input; loc; output]) ~tags:(stamp t.remote_id));
      
      command_loop t *)
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
      Bgp_log.info (fun m -> m "%s" (String.concat "\n" ["GC stat:"; allocation; size; collection]));
      
      command_loop id_map
    | s -> 
      Bgp_log.info (fun m -> m "Unrecognised command %s" s);
      command_loop id_map
  ;;
        
  let start s =
    (* Enable backtrace *)
    Printexc.record_backtrace true;

    (* Parse config from file *)
    let config = Config_parser.parse_from_file (Key_gen.config ()) in

    (* Init loc-rib *)
    let loc_rib = Rib.Loc_rib.create config in

    let local_port = 
      let open Config_parser in
      config.local_port
    in
    
    (* Init shared data *)
    let init_t peer =
      let open Config_parser in
      let stat = {
        sent_open = 0;
        sent_update = 0;
        sent_keepalive = 0;
        sent_notif = 0;
        rec_open = 0;
        rec_update = 0;
        rec_keepalive = 0;
        rec_notif = 0;
      } in
      {
        remote_id = peer.remote_id;
        remote_port = peer.remote_port;
        remote_asn = peer.remote_asn;

        local_id = config.local_id;
        local_port = config.local_port;
        local_asn = config.local_asn;

        socket = s; 
        fsm = Fsm.create 240 (peer.hold_time) (peer.hold_time / 3);
        stat;

        flow = None;
        conn_retry_timer = None; 
        hold_timer = None; 
        keepalive_timer = None;
        conn_starter = None;
        flow_reader = None;
        input_rib = None;
        output_rib = None;
        loc_rib;
      }
    in
    
    let t_list = 
      let open Config_parser in
      List.map init_t config.peers 
    in
    let id_map = List.fold_left (fun acc t -> Id_map.add t.remote_id t acc) (Id_map.empty) t_list in

    (* Start listening to BGP port. *)
    listen_tcp_connection s local_port id_map handle_event;
    
    (* Automatic start *)
    let f t =
      Bgp_log.info (fun m -> m "BGP starts." ~tags:(stamp t.remote_id));
      handle_event t Fsm.Automatic_start_passive_tcp
    in
    let _ = Id_map.map f id_map in

    command_loop id_map
  ;;
end