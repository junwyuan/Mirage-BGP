open Lwt.Infix

(* Logging *)
let bgpd_src = Logs.Src.create "bgp" ~doc:"BGP logging"
module Bgp_log = (val Logs.src_log bgpd_src : Logs.LOG)

(* Timer *)
module Timer = struct
  type t = unit Lwt.t
  let create time : t =
    let t, u = Lwt.task () in
    let _ = 
      OS.Time.sleep_ns (Duration.of_sec time) 
      >>= fun () -> 
      Lwt.wakeup u (); Lwt.return_unit 
    in
    t 
  ;;
end

module  Main (S: Mirage_stack_lwt.V4) = struct
  type flags = {
    mutable listen_tcp_conn: bool;
    mutable read_tcp_flow: bool;
  }

  type timers = {
    mutable conn_retry_timer: Timer.t option;
    mutable hold_timer: Timer.t option;
    mutable keepalive_timer: Timer.t option;
  }

  type t = {
    host_id: Ipaddr.V4.t;
    my_id: Ipaddr.V4.t;
    my_as: int;
    socket: S.t;
    mutable fsm: Fsm.t;
    mutable flow: S.TCPV4.flow option;
    flags: flags;
    timers: timers;
  }

  let rec msg_read_loop t callback =
    match t.flow with
    | None -> Lwt.return_unit
    | Some flow -> begin 
      S.TCPV4.read flow
      >>= fun result ->
      if not (t.flags.read_tcp_flow) then begin
        Bgp_log.debug (fun m -> m "Exit TCP read loop.");
        Lwt.return_unit (* Silent exit *)
      end
      else begin
        match result with
        | Ok (`Data buf) -> begin
          match Bgp.parse_buffer_to_t buf with
          | Error _ -> 
            (* TODO *)
            msg_read_loop t callback
          | Ok msg ->
            let event = match msg with
            | Bgp.Open o -> Fsm.BGP_open o
            | Bgp.Update u -> Fsm.Update_msg u
            | Bgp.Notification e -> Fsm.Notif_msg e
            | Bgp.Keepalive -> Fsm.Keepalive_msg
            in
            Bgp_log.info (fun m -> m "receive message %s" (Bgp.to_string msg));
            let _ = callback event in
            msg_read_loop t callback
        end 
      | Error _ -> 
        Bgp_log.debug (fun m -> m "fail to read from TCP.");
        callback Fsm.Tcp_connection_fail
      | Ok (`Eof) -> 
        Bgp_log.debug (fun m -> m "TCP connection closed by remote.");
        callback Fsm.Tcp_connection_fail
      end
    end
  ;;

  let init_tcp_connection t callback =
    Bgp_log.debug (fun m -> m "try setting up TCP connection with remote peer.");
    match t.flow with
    | Some _ -> Lwt.return_unit
    | None -> begin
      S.TCPV4.create_connection (S.tcpv4 t.socket) (t.host_id, 179)
      >>= function
      | Error _ -> 
        Bgp_log.debug (fun m -> m "fail to set up TCP connection.");
        callback (Fsm.Tcp_connection_fail)
      | Ok flow -> begin
        match t.flow with
        | None -> begin
          t.flow <- Some flow;
          t.flags.read_tcp_flow <- true;
          Bgp_log.debug (fun m -> m "TCP connection succeeds.");

          (* Spawn a thread to read messages from flow *)
          let _ = msg_read_loop t callback in

          callback Fsm.Tcp_CR_Acked
        end
        | Some _ -> S.TCPV4.close flow
      end
    end
  ;;      

  let listen_tcp_connection t callback =
    let on_connect = fun flow -> 
      if (t.flags.listen_tcp_conn) then begin
        t.flow <- Some flow;
        t.flags.read_tcp_flow <- true;
        Bgp_log.debug (fun m -> m "accept incoming TCP connection from peer.");

        (* Spawn a thread to read messages from flow *)
        let _ = msg_read_loop t callback in

        callback Fsm.Tcp_connection_confirmed
      end
      else Lwt.return_unit (* Silently quit *)
    in
    S.listen_tcpv4 t.socket ~port:179 on_connect
  ;;
                    
  let send_msg t msg = 
    match t.flow with
    | Some flow ->
      S.TCPV4.write flow (Bgp.gen_msg msg) 
      >>= begin function
      | Error _ ->
        Bgp_log.debug (fun m -> m "fail to write TCP message.");
        Lwt.return_unit
      | Ok () -> Lwt.return_unit
      end    
    | None -> Lwt.return_unit
  ;;

  let send_open_msg (t: t) =
    let open Bgp in
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 t.my_id;
      my_as = Asn t.my_as;
      hold_time = 180;
      options = [];
    } in
    send_msg t (Bgp.Open o) 
  ;;

  let drop_tcp_connection t =
    Bgp_log.debug (fun m -> m "Drop TCP connection.");
    t.flags.read_tcp_flow <- false;
    t.flags.listen_tcp_conn <- false;
    match t.flow with
    | None -> Lwt.return_unit
    | Some flow -> 
      t.flow <- None; 
      S.TCPV4.close flow;
  ;;

  let rec perform_action t action =
    let open Fsm in
    let callback = fun event -> handle_event t event in
    match action with
    | Initiate_tcp_connection -> init_tcp_connection t callback
    | Send_open_msg -> send_open_msg t
    | Send_msg msg -> send_msg t msg
    | Drop_tcp_connection -> drop_tcp_connection t
    | Listen_tcp_connection -> t.flags.listen_tcp_conn <- true; Lwt.return_unit
    | _ -> Lwt.return_unit

  and handle_event t event =
    let new_fsm, actions = Fsm.handle t.fsm event in
    t.fsm <- new_fsm;
    (* Spawn threads to perform actions from left to right *)
    let _ = List.fold_left (fun acc act -> List.cons (perform_action t act) acc) [] actions in
    Lwt.return_unit
  ;;

  let start_bgp host_id my_id my_as socket =
    let fsm = Fsm.create 120 90 30 in
    let flags = { listen_tcp_conn = false; read_tcp_flow = false } in
    let flow = None in
    let timers = {
      conn_retry_timer = None;
      hold_timer = None;
      keepalive_timer = None;
    } in
    let t = { host_id; my_id; my_as; socket; fsm; flow; flags; timers } in

    (* Start listening to BGP port. *)
    listen_tcp_connection t (handle_event t);

    (* This is the command loop *)
    let rec loop () =
      Lwt_io.read_line Lwt_io.stdin
      >>= function
      | "start" -> 
        Bgp_log.info (fun m -> m "BGP starts.");
        handle_event t (Fsm.Manual_start) >>= loop
      | "stop" -> 
        Bgp_log.info (fun m -> m "BGP stops.");
        handle_event t (Fsm.Manual_stop) >>= loop
      | "exit" -> 
        Bgp_log.info (fun m -> m "BGP exits.");
        S.disconnect socket
        >>= fun () ->
        Lwt.return_unit
      | _ -> Lwt.return_unit >>= loop
    in
    loop ()
  ;;

  let start s =
    let host = Ipaddr.V4.of_string_exn (Key_gen.host ()) in
    let id = Ipaddr.V4.of_string_exn (Key_gen.id ()) in
    let my_as = Key_gen.asn () in  
    start_bgp host id my_as s
  ;;


end