open Lwt.Infix

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

  let rec msg_read_loop t callback =
    match t.flow with
    | None -> Lwt.return_unit
    | Some flow -> begin 
      S.TCPV4.read flow
      >>= fun result -> 
      if not (t.flags.read_tcp_flow) then 
        let _ = C.log t.console "Debug: Exit tcp read loop." in
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
            let _ = callback event in
            msg_read_loop t callback
        end 
      | Error _ -> 
        let _ = C.log t.console "Debug: tcp read error in read_msg." in
        callback Fsm.Tcp_connection_fail
      | Ok (`Eof) -> 
        let _ = C.log t.console "Debug: connection closed by the other end." in
        callback Fsm.Tcp_connection_fail
      end
    end
  ;;

  let init_tcp_connection t callback =
    let _ = C.log t.console "Debug: try connect to the remote." in
    match t.flow with
    | Some _ -> Lwt.return_unit
    | None -> begin
      S.TCPV4.create_connection (S.tcpv4 t.socket) (t.host_id, 50000)
      >>= function
      | Error _ -> 
        let _ = C.log t.console "Debug: fail to connect to remote." in
        callback (Fsm.Tcp_connection_fail)
      | Ok flow -> begin
        match t.flow with
        | None -> begin
          t.flow <- Some flow;
          t.flags.read_tcp_flow <- true;
          let _ = C.log t.console "Debug: connected to remote." in

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
        let _ = C.log t.console "Debug: accept remote connection." in

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
        let _ = C.log t.console "Debug: fail to write message." in
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
    let _ = C.log t.console "Debug: drop tcp connection." in
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


  let start_bgp host_id my_id my_as socket console =
    let fsm = Fsm.create 120 90 30 in
    let flags = { listen_tcp_conn = false; read_tcp_flow = false } in
    let flow = None in
    let t = { host_id; my_id; my_as; socket; console; fsm; flow; flags; } in

    (* Start listening to BGP port. *)
    listen_tcp_connection t (handle_event t);

    (* This is the command loop *)
    let rec loop () =
      Lwt_io.read_line Lwt_io.stdin
      >>= function
      | "start" -> 
        let _ = C.log console "Debug: bgpd starts." in
        handle_event t (Fsm.Manual_start) >>= loop
      | "stop" -> 
        let _ = C.log console "Debug: bgpd stops." in
        handle_event t (Fsm.Manual_stop) >>= loop
      | "exit" -> 
        let _ = C.log console "Debug: exiting." in
        S.disconnect socket
        >>= fun () ->
        Lwt.return_unit
      | _ -> Lwt.return_unit >>= loop
    in
    loop ()
  ;;

  let start c s =
    let host = Ipaddr.V4.of_string_exn (Key_gen.host ()) in
    let id = Ipaddr.V4.of_string_exn (Key_gen.id ()) in
    let my_as = Key_gen.asn () in  
    start_bgp host id my_as s c
  ;;


end