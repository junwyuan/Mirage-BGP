open Lwt.Infix
open Printf
open Bgp

let rec_log = Logs.Src.create "receiver" ~doc:"Receiver log"
module Rec_log = (val Logs.src_log rec_log : Logs.LOG)

module  Main (S: Mirage_stack_lwt.V4) = struct
  module Bgp_flow = Bgp_io.Make(S)

  let start_time = Unix.gettimeofday ();;
  let time f c =
    let t = Sys.time () in
    f () >>= fun () ->
    Rec_log.info (fun m -> m "execution time: %fs\n" (Sys.time() -. t));
    Lwt.return_unit
  ;;
  
  let read_tcp_base flow callback = 
    Bgp_flow.read flow
    >>= fun read_result ->
    match read_result with 
    | Ok msg -> 
      Rec_log.info (fun m -> m "Receive: %s" (Bgp.to_string msg));
      callback ()
    | Error err ->
      (match err with 
      | `Refused -> Rec_log.debug (fun m -> m "Read refused")
      | `Timeout -> Rec_log.debug (fun m -> m "Read timeout")
      | `Closed -> Rec_log.debug (fun m -> m "Connection closed when read.")
      | `BGP_MSG_ERR err ->
        (match err with
        | Bgp.Parsing_error -> Rec_log.debug (fun m -> m "Parsing error")
        | _ -> Rec_log.debug (fun m -> m "Msg format error"))
      | _ -> ()); 
      Lwt.return_unit
  ;;

  let read_flow_once flow = 
    read_tcp_base flow (fun () -> Lwt.return_unit)
  ;;

  let rec read_flow_loop flow = 
    read_tcp_base flow (fun () -> read_flow_loop flow)
  ;;

  let write_msg flow = fun msg ->
    Rec_log.info (fun m -> m "send TCP message %s" (Bgp.to_string msg));
    Bgp_flow.write flow msg 
    >>= function
    | Error err ->
      (match err with
      | `Timeout -> Rec_log.info (fun m -> m "Write time out")
      | `Refused -> Rec_log.info (fun m -> m "Write refused.")
      | `Closed -> Rec_log.info (fun m -> m "Connection closed when write.") 
      | _ -> ());
      Lwt.fail_with "TCP FAIL"
    | Ok _ -> Lwt.return_unit
  ;;

  let rec write_keepalive flow =
    OS.Time.sleep_ns (Duration.of_sec 30) 
    >>= fun () ->
    write_msg flow Bgp.Keepalive
    >>= fun () ->
    write_keepalive flow
  ;;

  let ip4_of_ints a b c d =
    Int32.of_int ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
  ;;
  
  let write_update flow =
    let open Bgp in
    let withdrawn = 
      [(Afi.IPv4 (ip4_of_ints 192 168 0 0), 16); 
        (Afi.IPv4 (ip4_of_ints 10 0 0 0), 8); 
        (Afi.IPv4 (ip4_of_ints 172 16 84 0), 24);
        ] 
    in
    let nlri = [(Afi.IPv4 (ip4_of_ints 192 168 0 0), 24)] in
    let flags = {optional=false; transitive=true; partial=false; extlen=false} in
    let path_attrs = [
      flags, Origin IGP;
      flags, As_path [Set [2_l; 5_l; 3_l]; Seq [10_l; 20_l; 30_l]];
      flags, Next_hop (Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn (Key_gen.local_id ())));
    ] in 
    let u = Update {withdrawn; path_attrs; nlri} in
    write_msg flow u
  ;;

  let rec loop () = 
    OS.Time.sleep_ns (Duration.of_sec 60) 
    >>= fun () ->
    loop ()
  ;;

  let start_receive_passive flow =
    read_flow_once flow
    >>= fun () ->
    let open Bgp in
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn (Key_gen.local_id ()));
      my_as = Asn (Key_gen.local_asn ());
      options = [];
      hold_time = 180;
    } in
    write_msg flow (Bgp.Open o)
    >>= fun () ->
    read_flow_once flow
    >>= fun () ->
    write_msg flow (Bgp.Keepalive)
    >>= fun () ->
    Lwt.join [read_flow_loop flow; write_keepalive flow]
  ;;

  let start_receive_active flow =
    let open Bgp in
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn (Key_gen.local_id ()));
      my_as = Asn (Key_gen.local_asn ());
      options = [];
      hold_time = 180;
    } in
    write_msg flow (Bgp.Open o)
    >>= fun () ->
    read_flow_once flow 
    >>= fun () ->
    write_msg flow (Bgp.Keepalive)
    >>= fun () ->
    read_flow_loop flow
    >>= fun () ->
    Lwt.join [
      Lwt.catch (fun () -> read_flow_once flow) (fun exn -> Rec_log.err (fun m -> m "Some exn catched"); Lwt.return_unit);
      write_keepalive flow
    ]
  ;;

  let start s =
    let port = Key_gen.local_port () in
    Bgp_flow.listen s port (fun flow -> start_receive_passive flow);

    let _ = 
      Bgp_flow.create_connection s (Ipaddr.V4.of_string_exn (Key_gen.remote_id ()), Key_gen.remote_port ())
      >>= function
      | Error err -> 
        (match err with
        | `Timeout -> Rec_log.info (fun m -> m "Conn timeout")
        | `Refused -> Rec_log.info (fun m -> m "Conn refused")
        | _ -> ());
        Lwt.return_unit
      | Ok flow -> 
        Rec_log.info (fun m -> m "Connect to remote"); 
        start_receive_active flow
    in

    loop ()
  ;;
end



