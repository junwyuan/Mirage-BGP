open Lwt.Infix
open Bgp

(* Design choices: Use signal-and-continue *)

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

module Ip_map = Map.Make(Ipaddr.V4)

module  Make (S: Mirage_stack_lwt.V4) = struct
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
    iBGP: bool;

    remote_id: Ipaddr.V4.t;
    remote_port: int;
    remote_asn: int32;
    local_id: Ipaddr.V4.t;
    local_port: int;
    local_asn: int32;
    socket: S.t;

    inbound_filter: Filter.route_map option;

    (* Temporary flow storage *)
    mutable in_flow: Bgp_flow.t option;
    mutable out_flow: Bgp_flow.t option;

    mutable flow_handler: Flow_handler.t option;
    
    mutable fsm: Fsm.t;
    mutable conn_retry_timer: Device.t option;
    mutable hold_timer: Device.t option;
    mutable keepalive_timer: Device.t option;
    mutable conn_starter: Device.t option;

    mutable in_rib: Rib.Adj_rib_in.t option;
    out_rib: Rib.Adj_rib_out.t;
    loc_rib: Rib.Loc_rib.t;
    
    stat: stat;

    (* Logging *)
    log: (module Logs.LOG);

    stream: Fsm.event Lwt_stream.t;
    pf: Fsm.event option -> unit;

    (* Synchronisation *)
    cond_var: unit Lwt_condition.t;
  }

  let push_event t event = t.pf (Some event)

  let stop t =
    t.running <- false;
    t.pf None
  ;;

  let create_timer time callback : Device.t =
    Device.create (fun () -> OS.Time.sleep_ns (Duration.of_sec time)) callback
  ;;

  let listen_tcp_connection s local_port id_map =
    let on_connect flow =
      Bgp_flow.read flow >>= function
      | Error err ->
        let ip, _ = Bgp_flow.dst flow in
        let () = match err with
          | `PARSE_ERROR err ->  
            Logs.info (fun m -> m "receive an unknown connection from ip %s with err %s" 
                                      (Ipaddr.V4.to_string ip) (Bgp.parse_error_to_string err))
          | _ ->
            Logs.debug (fun m -> m "receive an unknown connection from ip %s, but failed" 
                                      (Ipaddr.V4.to_string ip))
        in
        Lwt.return_unit
      | Ok Open opent -> begin
        let ip, _ = Bgp_flow.dst flow in
        Logs.debug (fun m -> m "Receive incoming connection from %s(real) with open message %s " 
                                    (Ipaddr.V4.to_string ip) (Bgp.to_string (Open opent)));

        let remote_id = opent.local_id in
        let remote_asn = opent.local_asn in
        Logs.info (fun m -> m "Receive incoming connection from %s remote as %ld" 
                                (Ipaddr.V4.to_string remote_id) remote_asn);

        match Ip_map.mem remote_id id_map with
        | false -> 
          Logs.info (fun m -> m "Refuse connection because remote id %s is unconfigured." 
                                    (Ipaddr.V4.to_string remote_id));
          Bgp_flow.close flow
        | true -> begin
          let open Fsm in
          let t = Ip_map.find remote_id id_map in
          let module Log = (val t.log : Logs.LOG) in

          match t.fsm.state with
          | IDLE -> 
            Log.info (fun m -> m "Refuse connection %s because fsm is at IDLE." (Ipaddr.V4.to_string remote_id));
            (* Confusion with RFC4271: do I need to increment the connect retry counter?? *)
            let _ = Bgp_flow.write flow (Notification Cease) in
            Bgp_flow.close flow
          | CONNECT | ACTIVE ->
            t.in_flow <- Some flow;
            push_event t Tcp_connection_confirmed;

            push_event t (BGP_open opent);
            Lwt.return_unit
          | OPEN_SENT | OPEN_CONFIRMED -> 
            if (Ipaddr.V4.compare t.local_id t.remote_id > 0) then begin
              Log.info (fun m -> m "Collision detected and dump new connection.");
              let _ = Bgp_flow.write flow (Notification Cease) in
              Bgp_flow.close flow
            end
            else begin
              Log.info (fun m -> m "Collision detected and dump existing connection.");
              
              (* Drop connection *)
              push_event t Open_collision_dump;

              (* Recover *)
              push_event t Automatic_start_passive_tcp;

              t.in_flow <- Some flow;
              push_event t Tcp_connection_confirmed;

              push_event t (BGP_open opent);
              
              Lwt.return_unit
            end
          | ESTABLISHED -> 
            Log.info (fun m -> m "Connection collision detected and dump new connection.");
            let _ = Bgp_flow.write flow (Notification Cease) in
            Bgp_flow.close flow
        end
      end
      | Ok msg ->
        let ip, _ = Bgp_flow.dst flow in
        Logs.debug (fun m -> m "Receive message %s from unknown connection with ip %s" (Bgp.to_string msg) 
                                (Ipaddr.V4.to_string ip));
        Lwt.return_unit
    in
    Bgp_flow.listen s local_port on_connect
  ;;

  let init_tcp_connection t =
    let module Bgp_log = (val t.log : Logs.LOG) in
    Bgp_log.info (fun m -> m "try setting up TCP connection with remote");
    if not (t.conn_starter = None) then
      Bgp_log.warn (fun m -> m "new connection is initiated when there exists another conn starter.")
    else if not (t.flow_handler = None) then
      Bgp_log.warn (fun m -> m "new connection is initiated when there exists an old connection.")
    else begin
      let task = fun () ->
        Bgp_flow.create_connection t.socket (t.remote_id, t.remote_port)
      in
      let wrapped_callback result =
        t.conn_starter <- None;
        match result with
        | Error err ->
          let () = match err with
            | `Timeout -> 
              Bgp_log.info (fun m -> m "Connection init timeout.")
            | `Refused -> 
              Bgp_log.info (fun m -> m "Connection init refused.")
            | err -> 
              Bgp_log.debug (fun m -> m "Unknown connection init error %a." 
                                                S.TCPV4.pp_error err)
          in
          
          push_event t (Fsm.Tcp_connection_fail);
          Lwt.return_unit
        | Ok flow -> begin
          let open Fsm in
          Bgp_log.info (fun m -> m "Connected to remote %s" (Ipaddr.V4.to_string t.remote_id));
          match t.fsm.state with
          | IDLE -> 
            Bgp_log.info (fun m -> m "Drop connection to remote %s because fsm is at IDLE" 
                                        (Ipaddr.V4.to_string t.remote_id));
            Bgp_flow.close flow
          | CONNECT | ACTIVE ->
            t.out_flow <- Some flow;
            push_event t Tcp_CR_Acked;
            Lwt.return_unit
          | OPEN_SENT | OPEN_CONFIRMED -> 
            if (Ipaddr.V4.compare t.local_id t.remote_id < 0) then begin
              Bgp_log.info (fun m -> m "Connection collision detected and dump new connection." );
              Bgp_flow.close flow
            end
            else begin
              Bgp_log.info (fun m -> m "Connection collision detected and dump existing connection." );
              
              (* Drop connection *)
              push_event t Open_collision_dump;

              (* Recover *)
              push_event t Automatic_start_passive_tcp;

              t.out_flow <- Some flow;
              push_event t Tcp_CR_Acked;

              Lwt.return_unit
            end
          | ESTABLISHED -> 
            Bgp_log.info (fun m -> m "Connection collision detected and dump new connection.");
            Bgp_flow.close flow
        end
      in
      t.conn_starter <- Some (Device.create task wrapped_callback)
    end
  ;;      

                  
  let send_msg t msg = 
    let module Bgp_log = (val t.log : Logs.LOG) in  
    let () = match msg with
      | Open _ -> t.stat.sent_open <- t.stat.sent_open + 1
      | Update _ -> t.stat.sent_update <- t.stat.sent_update + 1
      | Keepalive -> t.stat.sent_keepalive <- t.stat.sent_keepalive + 1
      | Notification _ -> t.stat.sent_notif <- t.stat.sent_notif + 1
    in 

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
      options = [ Capability [ Mp_ext (Afi.IP4, Safi.UNICAST)] ];
    } in
    send_msg t (Bgp.Open o) 
  ;;

  let drop_tcp_connection t =
    let module Bgp_log = (val t.log : Logs.LOG) in 
    let () =    
      match t.conn_starter with
      | None -> ()
      | Some d -> 
        Bgp_log.debug (fun m -> m "close conn starter."); 
        t.conn_starter <- None;
        Device.stop d
    in

    let () =
      match t.flow_handler with
      | None -> ()
      | Some d ->
        Bgp_log.debug  (fun m -> m "close flow handler."); 
        t.flow_handler <- None;
        Flow_handler.stop d
    in

    ()
  ;;

  (* Design choices: I want all actions to be performed in a blocking way. *)
  (* No other thread can update the data structure before all actions are performed. *)
  let rec perform_action t action : unit =
    let open Fsm in
    let module Bgp_log = (val t.log : Logs.LOG) in 
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
      | Some d -> 
        t.conn_retry_timer <- None; 
        Device.stop d
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
      match t.in_rib with
      | None -> 
        Bgp_log.err (fun m -> m "Input RIB not initiated"); 
        failwith "Input RIB not initiated."
      | Some rib -> 
        Rib.Adj_rib_in.push_update rib u
    end
    | Initiate_rib ->
      let in_rib = 
        let callback (wd, ins) = 
          Rib.Loc_rib.push_update t.loc_rib (t.remote_id, wd, ins) 
        in
        let signal () = Lwt_condition.signal t.cond_var () in
        Rib.Adj_rib_in.create t.remote_id t.iBGP callback signal t.inbound_filter
      in
      t.in_rib <- Some in_rib;

      let callback u = 
        send_msg t (Bgp.Update u)
      in

      let () = 
        let open Rib.Adj_rib_out in
        t.out_rib.callbacks <- Ip_map.add t.remote_id callback t.out_rib.callbacks
      in
      
      Rib.Loc_rib.sub t.loc_rib (t.remote_id, t.remote_asn, in_rib, t.out_rib, callback)
    | Release_rib ->
      let () =
        match t.in_rib with
        | None -> ()
        | Some rib ->
          t.in_rib <- None;
          Rib.Adj_rib_in.stop rib
      in

      let () = 
        let open Rib.Adj_rib_out in
        t.out_rib.callbacks <- Ip_map.remove t.remote_id t.out_rib.callbacks
      in

      ()
  ;;    
  
  let rec handle_event_loop t =
    if not t.running then Lwt.return_unit
    else
      let router_handle = function
        | None -> Lwt.return_unit
        | Some event ->
          let module Bgp_log = (val t.log : Logs.LOG) in 
          let () = match event with
            | Fsm.BGP_open _ -> t.stat.rec_open <- t.stat.rec_open + 1
            | Fsm.Update_msg _ -> t.stat.rec_update <- t.stat.rec_update + 1
            | Fsm.Keepalive_msg -> t.stat.rec_keepalive <- t.stat.rec_keepalive + 1
            | Fsm.Notif_msg _ -> t.stat.rec_notif <- t.stat.rec_notif + 1
            | _ -> ()
          in
          
          let new_fsm, actions = Fsm.handle t.fsm event in

          (* Update finite state machine before performing any action. *)
          t.fsm <- new_fsm;

          (* Initiate flow handler *)
          let () = match event with
            | Fsm.Tcp_CR_Acked -> begin
              match t.out_flow with
              | None -> Bgp_log.err (fun m -> m "In_flow missing.")
              | Some flow ->
                let flow_handler = Flow_handler.create t.remote_id t.remote_asn (push_event t) flow t.log in
                t.flow_handler <- Some flow_handler;
                t.out_flow <- None
            end
            | Fsm.Tcp_connection_confirmed -> begin
              match t.in_flow with
              | None -> Bgp_log.err (fun m -> m "Out_flow missing.")
              | Some flow ->
                let flow_handler = Flow_handler.create t.remote_id t.remote_asn (push_event t) flow t.log in
                t.flow_handler <- Some flow_handler;
                t.in_flow <- None
            end
            | _ -> ()
          in

          (* Blocking perform all actions *)
          let () =
            let f t act = perform_action t act in
            List.iter (f t) actions
          in

          (* Wait for In RIB if IN RIB is released *)
          let wthread = 
            if List.mem Fsm.Release_rib actions then
              Lwt_condition.wait t.cond_var
            else Lwt.return_unit
          in
          wthread >>= fun () ->

          (* Unsub from Loc RIB *)
          let () = 
            if List.mem Fsm.Release_rib actions then
              Rib.Loc_rib.unsub t.loc_rib t.remote_id
            else () 
          in
            
          match Fsm.state t.fsm with
          | Fsm.IDLE -> begin
            match event with
            | Fsm.Manual_stop -> handle_event_loop t
            | _ ->
              (* Automatic restart *)
              Bgp_log.info (fun m -> m "BGP automatic restarts.");
              push_event t Fsm.Automatic_start_passive_tcp;
              handle_event_loop t
          end
          | _ -> handle_event_loop t
      in
      Lwt_stream.get t.stream >>= fun res ->
      router_handle res
  ;;

  let create socket loc_rib out_rib config peer_config =
    let open Config_parser in
    
    (* Init shared data *)
    let stream, pf = Lwt_stream.create () in

    let bgpd_src = Logs.Src.create (Ipaddr.V4.to_string peer_config.remote_id) ~doc:"BGP logging" in

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

      socket; 
      conn_starter = None;
      flow_handler = None;
      in_flow = None;
      out_flow = None;

      remote_id = peer_config.remote_id;
      remote_port = peer_config.remote_port;
      remote_asn = peer_config.remote_asn;
      local_id = config.local_id;
      local_port = config.local_port;
      local_asn = config.local_asn;
      
      iBGP = config.local_asn = peer_config.remote_asn;
      inbound_filter = peer_config.inbound_filter;
      
      fsm = Fsm.create peer_config.conn_retry_time (peer_config.hold_time) (peer_config.hold_time / 3);

      conn_retry_timer = None; 
      hold_timer = None; 
      keepalive_timer = None;

      in_rib = None;
      out_rib;
      loc_rib;

      stat;
      log = Logs.src_log bgpd_src;

      stream; pf;

      cond_var = Lwt_condition.create ();
    } in

    let _ = handle_event_loop t in

    t
  ;;
end
