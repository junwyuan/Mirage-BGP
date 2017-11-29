open Lwt.Infix
open Printf

module  Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4) = struct
  module Thread_map = Map.Make(String);;
  module FSM = Fsm.Make(S);;

  type flags = {
    mutable listen_tcp_conn: bool;
    mutable read_tcp_flow: bool;
  }

  type t = {
    mutable fsm: FSM.t;
    flags: flags;
  }

  let init_tcp_connection host soc push =
    S.TCPV4.create_connection soc (host, 179)
    >>= (function
    | Error _ -> Lwt.return FSM.Tcp_connection_fail
    | Ok flow -> Lwt.return (FSM.Tcp_CR_Acked flow))
    >>= fun event ->
    let () = push (Some event) in
    Lwt.return_unit
  ;;      

  let send_msg flow msg = 
    match flow with
    | Some flow ->
      S.TCPV4.write flow (Bgp.gen_msg msg) 
      >>= (function
      | Error _ -> printf "Write Error"; Lwt.return_unit
      | Ok () -> Lwt.return_unit)
    | None -> Lwt.return_unit
  ;;

  let send_open_msg id my_as flow =
    let open Bgp in
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 id;
      my_as = Asn my_as;
      hold_time = 180;
      options = [];
    } in
    send_msg flow (Bgp.Open o) 
  ;;

  let drop_tcp_connection flow =
    match flow with
    | None -> Lwt.return_unit
    | Some flow -> S.TCPV4.close flow
  ;;

  let rec bgp_daemon host id my_as fsm soc events push threads flow =
    Lwt_stream.get events 
    >>= function
    | None -> bgp_daemon host id my_as fsm soc events push threads flow
    | Some event ->
      let open FSM in
      let new_fsm, actions = handle fsm event in
      let perform_action = function
        | Initiate_tcp_connection -> init_tcp_connection host (S.tcpv4 soc) push
        | Send_open_msg -> send_open_msg id my_as flow
        | Send_msg msg -> send_msg flow msg
        | Drop_tcp_connection -> drop_tcp_connection flow
      in
      Lwt.join (List.map perform_action actions)
      >>= fun () ->
      bgp_daemon host id my_as fsm soc events push threads flow
  ;;

  let start _c s =
    let host = Ipaddr.V4.of_string_exn (Key_gen.host ()) in
    let id = Ipaddr.V4.of_string_exn (Key_gen.id ()) in
    let asn = Key_gen.asn () in
    let fsm = FSM.create 60. 180. 30. in
    let soc = s in

    (* Produce event queue *)
    let events, push = Lwt_stream.create () in
    push (Some FSM.Manual_start);

    (* Listen to port 179 *)
    S.listen_tcpv4 soc ~port:179 (fun flow -> push (Some (FSM.Tcp_connection_confirmed flow)); Lwt.return_unit);
    

    let threads = Thread_map.empty in
    bgp_daemon host id asn fsm soc events push threads None
  ;;


end