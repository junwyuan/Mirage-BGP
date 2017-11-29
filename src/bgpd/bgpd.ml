open Lwt.Infix
open Printf

module  Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4) = struct
  type flags = {
    mutable listen_tcp_conn: bool;
    mutable read_tcp_flow: bool;
  }

  type t = {
    host_id: Ipaddr.V4.t;
    my_id: Ipaddr.V4.t;
    my_as: int;
    socket: S.t;
    console: C.t;
    mutable fsm: Fsm.t;
    mutable flow: S.TCPV4.flow option;
    flags: flags;
  }

  let read_tcp_msg flow console = 
    S.TCPV4.read flow 
    >>= function
    | Ok (`Data buf) -> Lwt.return buf
    | Error _ -> 
      C.log console "tcp read error in read_msg" 
      >>= fun () -> 
      Lwt.fail_with "tcp read error in read_msg"
    | Ok (`Eof) -> 
      C.log console "Connection closed by the other end."
      >>= fun () -> 
      Lwt.fail_with "Connection closed by the other end."
  ;;

  let rec msg_read_loop t callback =
    match t.flow with
    | None -> Lwt.return_unit
    | Some flow -> begin 
      S.TCPV4.read flow
      >>= fun result -> 
      if not (t.flags.read_tcp_flow) then 
        C.log t.console "Debug: Exit tcp read loop."
        >>= fun () ->
        Lwt.return_unit (* Silent exit *)
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
            Lwt.join [msg_read_loop t callback; callback event]
        end 
      | Error _ -> 
        C.log t.console "Debug: tcp read error in read_msg." 
        >>= fun () -> 
        callback Fsm.Tcp_connection_fail
      | Ok (`Eof) -> 
        C.log t.console "Debug: connection closed by the other end."
        >>= fun () -> 
        callback Fsm.Tcp_connection_fail
      end
    end
  ;;

  let init_tcp_connection t callback =
    C.log t.console "Debug: try connect to the remote."
    >>= fun () ->
    match t.flow with
    | Some _ -> Lwt.return_unit
    | None -> begin
      S.TCPV4.create_connection (S.tcpv4 t.socket) (t.host_id, 50000)
      >>= function
      | Error _ -> 
        C.log t.console "Debug: fail to connect to remote."
        >>= fun () ->
        callback (Fsm.Tcp_connection_fail)
      | Ok flow -> begin
        match t.flow with
        | None -> begin
          t.flow <- Some flow;
          t.flags.read_tcp_flow <- true;
          C.log t.console "Debug: connected to remote."
          >>= fun () ->
          Lwt.join [callback Fsm.Tcp_CR_Acked; msg_read_loop t callback]
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
        Lwt.join [callback Fsm.Tcp_connection_confirmed; msg_read_loop t callback]
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
        C.log t.console "Debug: fail to write message."
        >>= fun () -> 
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
    C.log t.console "Debug: drop tcp connection."
    >>= fun () ->
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
    | Initiate_tcp_connection ->
      init_tcp_connection t callback
    | Send_open_msg -> send_open_msg t
    | Send_msg msg -> send_msg t msg
    | Drop_tcp_connection -> drop_tcp_connection t
    | Listen_tcp_connection -> t.flags.listen_tcp_conn <- true; Lwt.return_unit
  and handle_event t event =
    let open Fsm in
    let new_fsm, actions = handle t.fsm event in
    t.fsm <- new_fsm;
    Lwt.join (List.map (perform_action t) actions)
  ;;

  let start_bgp host_id my_id my_as socket console =
    let fsm = Fsm.create 60. 180. 30. in
    let flags = { listen_tcp_conn = false; read_tcp_flow = false } in
    let flow = None in
    let t = { host_id; my_id; my_as; socket; console; fsm; flow; flags; } in
    listen_tcp_connection t (handle_event t);
    C.log console "Debug: bgpd start."
    >>= fun () ->
    handle_event t (Fsm.Manual_start)
  ;;

  let start c s =
    let host = Ipaddr.V4.of_string_exn (Key_gen.host ()) in
    let id = Ipaddr.V4.of_string_exn (Key_gen.id ()) in
    let my_as = Key_gen.asn () in  
    start_bgp host id my_as s c
  ;;


end