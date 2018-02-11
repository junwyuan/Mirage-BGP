open Lwt.Infix
open Bgp

(* Design choices: Use signal-and-continue *)

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



module  Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4) = struct
  module Id_map = Map.Make(Ipaddr.V4)
  module Bgp_flow = Bgp_io.Make(S)
  module Flow_handler = Flow_handler.Make(S)

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
    mutable running: bool;

    remote_id: Ipaddr.V4.t;
    remote_port: int;
    remote_asn: int32;
    local_id: Ipaddr.V4.t;
    local_port: int;
    local_asn: int32;
    socket: S.t;
    
    mutable flow_handler: Flow_handler.t option;
    
    mutable fsm: Fsm.t;
    mutable conn_retry_timer: Device.t option;
    mutable hold_timer: Device.t option;
    mutable keepalive_timer: Device.t option;
    mutable conn_starter: Device.t option;

    mutable input_rib: Rib.Adj_rib_in.t option;
    mutable output_rib: Rib.Adj_rib_out.t option;
    mutable loc_rib: Rib.Loc_rib.t;
    
    stat: stat;

    stream: Fsm.event Lwt_stream.t;
    pf: Fsm.event option -> unit;
  }

  let push_event t event = t.pf (Some event)

  let stop t =
    t.running <- false;
    t.pf None
  ;;

  let create_timer time callback : Device.t =
    Device.create (fun () -> OS.Time.sleep_ns (Duration.of_sec time)) callback
  ;;

  let init_tcp_connection peer =
    Bgp_log.info (fun m -> m "try setting up TCP connection with remote" ~tags:(stamp peer.remote_id));
    if not (peer.conn_starter = None) then
      Bgp_log.warn (fun m -> m "new connection is initiated when there exists another conn starter." ~tags:(stamp peer.remote_id))
    else if not (peer.flow_handler = None) then
      Bgp_log.warn (fun m -> m "new connection is initiated when there exists an old connection." ~tags:(stamp peer.remote_id))
    else begin
      let task = fun () ->
        Bgp_flow.create_connection peer.socket (peer.remote_id, peer.remote_port)
      in
      let wrapped_callback result =
        peer.conn_starter <- None;
        match result with
        | Error err ->
          let () = match err with
            | `Timeout -> 
              Bgp_log.info (fun m -> m "Connection init timeout." ~tags:(stamp peer.remote_id))
            | `Refused -> 
              Bgp_log.info (fun m -> m "Connection init refused." ~tags:(stamp peer.remote_id))
            | err -> 
              Bgp_log.info (fun m -> m "Unknown connection init error %a." 
                                                S.TCPV4.pp_error err ~tags:(stamp peer.remote_id))
          in
          
          push_event peer (Fsm.Tcp_connection_fail);
          Lwt.return_unit
        | Ok flow -> begin
          let open Fsm in
          Bgp_log.info (fun m -> m "Connected to remote %s" (Ipaddr.V4.to_string peer.remote_id));
          match peer.fsm.state with
          | IDLE -> 
            Bgp_log.info (fun m -> m "Drop connection to remote %s because fsm is at IDLE" 
                                        (Ipaddr.V4.to_string peer.remote_id));
            Bgp_flow.close flow
          | CONNECT | ACTIVE ->
            push_event peer Tcp_CR_Acked;
            
            let flow_handler = Flow_handler.create peer.remote_id peer.remote_asn (push_event peer) flow in
            peer.flow_handler <- Some flow_handler;
            
            Lwt.return_unit
          | OPEN_SENT | OPEN_CONFIRMED -> 
            if (Ipaddr.V4.compare peer.local_id peer.remote_id < 0) then begin
              Bgp_log.info (fun m -> m "Connection collision detected and dump new connection." 
                                                                ~tags:(stamp peer.remote_id));
              Bgp_flow.close flow
            end
            else begin
              Bgp_log.info (fun m -> m "Connection collision detected and dump existing connection." 
                                                                ~tags:(stamp peer.remote_id));
              
              (* Drop connection *)
              push_event peer Open_collision_dump;
              
              (* Reset finite machine *)
              let new_fsm = {
                state = CONNECT;
                conn_retry_counter = peer.fsm.conn_retry_counter;
                conn_retry_time = peer.fsm.conn_retry_time;
                hold_time = peer.fsm.hold_time;
                keepalive_time = peer.fsm.keepalive_time;
              } in
              peer.fsm <- new_fsm;

              push_event peer Tcp_CR_Acked;

              let flow_handler = Flow_handler.create peer.remote_id peer.remote_asn (push_event peer) flow in
              peer.flow_handler <- Some flow_handler;

              Lwt.return_unit
            end
          | ESTABLISHED -> 
            Bgp_log.info (fun m -> m "Connection collision detected and dump new connection.");
            Bgp_flow.close flow
        end
      in
      peer.conn_starter <- Some (Device.create task wrapped_callback)
    end
  ;;      

  let listen_tcp_connection s local_port id_map =
    let on_connect flow =
      Bgp_flow.read flow >>= function
      | Error err ->
        let ip, _ = Bgp_flow.dst flow in
        let () = match err with
          | `PARSE_ERROR err ->  
            Bgp_log.debug (fun m -> m "receive an unknown connection from ip %s with err %s" 
                                      (Ipaddr.V4.to_string ip) (Bgp.parse_error_to_string err))
          | _ ->
            Bgp_log.debug (fun m -> m "receive an unknown connection from ip %s, but failed" 
                                      (Ipaddr.V4.to_string ip))
        in
        Lwt.return_unit
      | Ok Open opent -> begin
        Bgp_log.debug (fun m -> m "Receive incoming connection with open message %s" 
                                    (Bgp.to_string (Open opent)));

        let remote_id = opent.local_id in
        let remote_asn = opent.local_asn in
        Bgp_log.info (fun m -> m "Receive incoming connection from remote %s as %ld" 
                                (Ipaddr.V4.to_string remote_id) remote_asn);

        match Id_map.mem remote_id id_map with
        | false -> 
          Bgp_log.info (fun m -> m "Refuse connection because remote id %s is unconfigured." 
                                    (Ipaddr.V4.to_string remote_id));
          Bgp_flow.close flow
        | true -> begin
          let open Fsm in
          let peer = Id_map.find remote_id id_map in
          match peer.fsm.state with
          | IDLE -> 
            Bgp_log.info (fun m -> m "Refuse connection %s because fsm is at IDLE." (Ipaddr.V4.to_string remote_id));
            (* Confusion with RFC4271: do I need to increment the connect retry counter?? *)
            let _ = Bgp_flow.write flow (Notification Cease) in
            Bgp_flow.close flow
          | CONNECT | ACTIVE ->
            push_event peer Tcp_connection_confirmed;
            push_event peer (BGP_open opent);

            let flow_handler = Flow_handler.create peer.remote_id peer.remote_asn (push_event peer) flow in
            peer.flow_handler <- Some flow_handler;

            Lwt.return_unit
          | OPEN_SENT | OPEN_CONFIRMED -> 
            if (Ipaddr.V4.compare peer.local_id peer.remote_id > 0) then begin
              Bgp_log.info (fun m -> m "Collision detected and dump new connection.");
              let _ = Bgp_flow.write flow (Notification Cease) in
              Bgp_flow.close flow
            end
            else begin
              Bgp_log.info (fun m -> m "Collision detected and dump existing connection.");
              
              (* Drop connection *)
              push_event peer Open_collision_dump;

              (* Reset fsm *)
              let new_fsm = {
                state = CONNECT;
                conn_retry_counter = peer.fsm.conn_retry_counter;
                conn_retry_time = peer.fsm.conn_retry_time;
                hold_time = peer.fsm.hold_time;
                keepalive_time = peer.fsm.keepalive_time;
              } in
              peer.fsm <- new_fsm;
              
              push_event peer Tcp_connection_confirmed;
              push_event peer (BGP_open opent);

              let flow_handler = Flow_handler.create peer.remote_id peer.remote_asn (push_event peer) flow in
              peer.flow_handler <- Some flow_handler; 
              
              Lwt.return_unit
            end
          | ESTABLISHED -> 
            Bgp_log.info (fun m -> m "Connection collision detected and dump new connection.");
            let _ = Bgp_flow.write flow (Notification Cease) in
            Bgp_flow.close flow
        end
      end
      | Ok msg ->
        let ip, _ = Bgp_flow.dst flow in
        Bgp_log.debug (fun m -> m "Receive message %s from unknown connection with ip %s" (Bgp.to_string msg) 
                                (Ipaddr.V4.to_string ip));
        Lwt.return_unit
    in
    Bgp_flow.listen s local_port on_connect
  ;;
                    
  let send_msg t msg =   
    match t.flow_handler with
    | None -> Bgp_log.debug (fun m -> m "Flow handler is missing.")
    | Some handler -> Flow_handler.write handler msg
  ;;

  let send_open_msg (t: t) =
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
    let () =    
      match t.conn_starter with
      | None -> ()
      | Some d -> 
        Bgp_log.debug (fun m -> m "close conn starter." ~tags:(stamp t.remote_id)); 
        t.conn_starter <- None;
        Device.stop d
    in

    let () =
      match t.flow_handler with
      | None -> ()
      | Some d ->
        Bgp_log.debug  (fun m -> m "close flow handler." ~tags:(stamp t.remote_id)); 
        t.flow_handler <- None;
        Flow_handler.stop d
    in

    ()
  ;;

  (* Design choices: I want all actions to be performed in a blocking way. *)
  (* No other thread can update the data structure before all actions are performed. *)
  let rec perform_action t action : unit =
    let open Fsm in
    match action with
    | Initiate_tcp_connection -> init_tcp_connection t
    | Send_open_msg -> send_open_msg t
    | Send_msg msg -> send_msg t msg
    | Drop_tcp_connection -> drop_tcp_connection t
    | Start_conn_retry_timer -> 
      if (t.fsm.conn_retry_time > 0) then
        let callback () =
          t.conn_retry_timer <- None;
          push_event t Connection_retry_timer_expired
        in
        t.conn_retry_timer <- Some (create_timer t.fsm.conn_retry_time callback)
    | Stop_conn_retry_timer -> begin
      match t.conn_retry_timer with
      | None -> ()
      | Some d -> t.conn_retry_timer <- None; Device.stop d
    end
    | Reset_conn_retry_timer -> begin
      let () =
        match t.conn_retry_timer with
        | None -> ()
        | Some t -> Device.stop t
      in
      if (t.fsm.conn_retry_time > 0) then
        let callback () =
          t.conn_retry_timer <- None;
          push_event t Connection_retry_timer_expired
        in
        t.conn_retry_timer <- Some (create_timer t.fsm.conn_retry_time callback)
    end
    | Start_hold_timer ht -> 
      if (t.fsm.hold_time > 0) then 
        t.hold_timer <- Some (create_timer ht (fun () -> t.hold_timer <- None; push_event t Hold_timer_expired))
    | Stop_hold_timer -> begin
      match t.hold_timer with
      | None -> ()
      | Some d -> t.hold_timer <- None; Device.stop d
    end
    | Reset_hold_timer ht -> begin
      let () =
        match t.hold_timer with
        | None -> ()
        | Some d -> Device.stop d
      in
      if (t.fsm.hold_time > 0) then 
        t.hold_timer <- Some (create_timer ht (fun () -> t.hold_timer <- None; push_event t Hold_timer_expired))
    end
    | Start_keepalive_timer -> 
      if (t.fsm.keepalive_time > 0) then 
        t.keepalive_timer <- Some (create_timer t.fsm.keepalive_time 
                            (fun () -> t.keepalive_timer <- None; push_event t Keepalive_timer_expired))
    | Stop_keepalive_timer -> begin
      match t.keepalive_timer with
      | None -> ()
      | Some d -> t.keepalive_timer <- None; Device.stop d
    end
    | Reset_keepalive_timer -> begin
      let () =
        match t.keepalive_timer with
        | None -> ()
        | Some t -> Device.stop t
      in
      if (t.fsm.keepalive_time > 0) then 
        t.keepalive_timer <- Some (create_timer t.fsm.keepalive_time 
                            (fun () -> t.keepalive_timer <- None; push_event t Keepalive_timer_expired))
    end
    | Process_update_msg u -> begin
      match t.input_rib with
      | None -> 
        Bgp_log.err (fun m -> m "Input RIB not initiated"); 
        failwith "Input RIB not initiated."
      | Some rib -> 
        Rib.Adj_rib_in.push_update rib u
    end
    | Initiate_rib ->
      let in_rib = 
        let callback u = 
          Rib.Loc_rib.push_update t.loc_rib (t.remote_id, u);
        in
        Rib.Adj_rib_in.create t.remote_id callback
      in
      t.input_rib <- Some in_rib;

      let out_rib =
        let callback u = 
          send_msg t (Bgp.Update u)
        in
        Rib.Adj_rib_out.create t.remote_id callback
      in
      t.output_rib <- Some out_rib;

      Rib.Loc_rib.sub t.loc_rib (t.remote_id, in_rib, out_rib)
    | Release_rib ->
      let () =
        match t.input_rib with
        | None -> ()
        | Some rib ->
          t.input_rib <- None;
          Rib.Adj_rib_in.stop rib
      in

      let () = 
        match t.output_rib with
        | None -> ()
        | Some rib -> 
          t.output_rib <- None; 
          Rib.Adj_rib_out.stop rib
      in

      Rib.Loc_rib.unsub t.loc_rib t.remote_id
  ;;    
  
  let rec handle_event_loop t =
    if not t.running then Lwt.return_unit
    else begin
      Lwt_stream.get t.stream
      >>= function
      | None -> Lwt.return_unit
      | Some event ->
        let new_fsm, actions = Fsm.handle t.fsm event in
        
        (* Update finite state machine before performing any action. *)
        t.fsm <- new_fsm;

        (* Blocking perform all actions *)
        let () =
          let f t act = perform_action t act in
          List.iter (f t) actions
        in
          
        match Fsm.state t.fsm with
        | Fsm.IDLE -> begin
          match event with
          | Fsm.Manual_stop -> Lwt.return_unit
          | _ ->
            (* Automatic restart *)
            Bgp_log.info (fun m -> m "BGP automatic restarts." ~tags:(stamp t.remote_id));
            push_event t Fsm.Automatic_start_passive_tcp;
            handle_event_loop t
        end
        | _ -> handle_event_loop t
    end
  ;;

  let rec command_loop console peers =
    C.read console >>= function
    | Error err -> 
      Bgp_log.warn (fun m -> m "Fail to read command: %a" C.pp_error err);
      Lwt.return_unit
    | Ok (`Eof) ->
      Bgp_log.warn (fun m -> m "Console closed?");
      Lwt.return_unit
    | Ok (`Data b) ->
      match String.trim @@ Cstruct.to_string b with
      | "start" -> 
        let f t =
          Bgp_log.info (fun m -> m "BGP starts." ~tags:(stamp t.remote_id));
          push_event t (Fsm.Manual_start)
        in
        let _ = Id_map.map f peers in
        command_loop console peers
      | "stop" -> 
        let f t =
          Bgp_log.info (fun m -> m "BGP stops." ~tags:(stamp t.remote_id));
          push_event t (Fsm.Manual_stop)
        in
        let _ = Id_map.map f peers in
        command_loop console peers
      | "show peers" ->
        let f t =
          let fsm = Printf.sprintf "FSM: %s" (Fsm.to_string t.fsm) in
          let running_dev =
            let dev1 = if not (t.conn_retry_timer = None) then "Conn retry timer" else "" in
            let dev2 = if not (t.hold_timer = None) then "Hold timer" else "" in 
            let dev3 = if not (t.keepalive_timer = None) then "Keepalive timer" else "" in 
            let dev4 = if not (t.conn_starter = None) then "Conn starter" else "" in 
            let dev5 = if not (t.flow_handler = None) then "Flow handler" else "" in 
            let str_list = List.filter (fun x -> not (x = "")) [dev1; dev2; dev3; dev4; dev5] in
            Printf.sprintf "Running Dev: %s" (String.concat "; " str_list)
          in
          let msgs = 
            Printf.sprintf "Sent: %d OPEN; %d UPDATE; %d NOTIF; %d KEEPALIVE; \n Rec: %d OPEN; %d UPDATE; %d NOTIF; %d KEEPALIVE"
                          t.stat.sent_open t.stat.sent_update t.stat.sent_notif t.stat.sent_keepalive
                          t.stat.rec_open t.stat.rec_update t.stat.rec_notif t.stat.rec_keepalive
          in
          Bgp_log.info (fun m -> m "%s \n %s" (Ipaddr.V4.to_string t.remote_id) (String.concat "\n" [fsm; running_dev; msgs]));
        in
        let _ = Id_map.map f peers in
        command_loop console peers
      | "show routes" ->
        let () = 
          match !Rib.Loc_rib.t_ref with
          | None -> Bgp_log.info (fun m -> m "Empty")
          | Some rib -> Bgp_log.info (fun m -> m "%s" (Rib.Loc_rib.to_string rib))
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
        Bgp_log.info (fun m -> m "%s" (String.concat "\n" ["GC stat:"; allocation; size; collection]));
        
        command_loop console peers
      | "exit" ->
        Lwt.return_unit
      | str -> 
        Bgp_log.info (fun m -> m "Unrecognised command %s" str);
        command_loop console peers
  ;;

  (* let parse_config kv =
    let key = Key_gen.config () in
    KV.size kv key >>= function
    | Error e -> 
      Bgp_log.err (fun f -> f "Could not read config file: %a" KV.pp_error e);
      Lwt.return Config_parser.default_config
    | Ok size -> 
      KV.read kv key 0L size >>= function
      | Error e ->
        Bgp_log.err (fun f -> f "Could not read config file: %a" KV.pp_error e);
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
    let config = Config_parser.default_config in

    (* Init loc-rib *)
    let loc_rib = Rib.Loc_rib.create config.local_id config.local_asn in

    let local_port = config.local_port in
    
    (* Init shared data *)
    let init_t peer =
      let stream, pf = Lwt_stream.create () in

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

      let t = {
        running = true;

        socket = s; 
        conn_starter = None;
        flow_handler = None;

        remote_id = peer.remote_id;
        remote_port = peer.remote_port;
        remote_asn = peer.remote_asn;
        local_id = config.local_id;
        local_port = config.local_port;
        local_asn = config.local_asn;

        fsm = Fsm.create 240 (peer.hold_time) (peer.hold_time / 3);

        conn_retry_timer = None; 
        hold_timer = None; 
        keepalive_timer = None;

        input_rib = None;
        output_rib = None;
        loc_rib;

        stat;

        stream; pf;
      } in

      let _ = handle_event_loop t in

      t
    in
    
    let t_list = List.map init_t config.peers in
    let peers = List.fold_left (fun acc (t: t) -> Id_map.add t.remote_id t acc) (Id_map.empty) t_list in

    (* Start listening to BGP port. *)
    listen_tcp_connection s local_port peers;
    
    (* Automatic passive start *)
    let f _id (t: t) =
      Bgp_log.info (fun m -> m "BGP starts." ~tags:(stamp t.remote_id));
      push_event t Fsm.Automatic_start_passive_tcp
    in
    let () = Id_map.iter f peers in

    command_loop console peers
    >>= fun () ->

    (* Clean up *)
    S.disconnect s
  ;;
end