open Lwt.Infix
open Printf
open Bgp

let rec_log = Logs.Src.create "receiver" ~doc:"Receiver log"
module Rec_log = (val Logs.src_log rec_log : Logs.LOG)

module  Main (S: Mirage_stack_lwt.V4) = struct
  module Bgp_flow = Bgp_io.Make(S)

  let remote_id () = Ipaddr.V4.of_string_exn "127.0.0.1"

  let local_id () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2.id
    | _ -> Relay.dev_relay2.id
  ;;

  let remote_port () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2.port
    | _ -> Relay.dev_relay2.port
  ;;

  let local_asn () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2.as_no
    | _ -> Relay.dev_relay2.as_no
  ;;

  let count = ref 0

  let read_tcp_base flow callback = 
    Bgp_flow.read flow
    >>= fun read_result ->
    match read_result with 
    | Ok msg -> 
      count := !count + 1;
      Rec_log.debug (fun m -> m "Receive pkg %d: %s" (!count) (Bgp.to_string msg));
      callback msg
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
    read_tcp_base flow (fun msg -> Lwt.return_unit)
  ;;

  let rec read_flow_loop flow = 
    let marker = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn "10.0.0.0") in
    let is_marker ip = 
      match ip with
      | Afi.IPv6 _ -> false
      | Afi.IPv4 v -> Int32.compare v marker == 0
    in
    let rec callback msg = 
      match msg with
      | Update {withdrawn; nlri; path_attrs} ->
        if (List.exists (fun (ip, _) -> is_marker ip) nlri) then Lwt.return_unit
        else read_tcp_base flow callback
      | _ -> read_tcp_base flow callback
    in
    read_tcp_base flow callback
  ;;

  let write_msg flow = fun msg ->
    Rec_log.debug (fun m -> m "send TCP message %s" (Bgp.to_string msg));
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
      flags, Next_hop (Ipaddr.V4.to_int32 (local_id ()));
    ] in 
    let u = Update {withdrawn; path_attrs; nlri} in
    write_msg flow u
  ;;

  let rec loop () = 
    OS.Time.sleep_ns (Duration.of_sec 60) 
    >>= fun () ->
    loop ()
  ;;

  (* let start_receive_passive flow =
    Rec_log.info (fun m -> m "Accept incoming connection from remote.");
    read_flow_once flow
    >>= fun () ->
    let open Bgp in
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn "172.19.10.2");
      my_as = Asn (local_asn ());
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
  ;; *)

  let start_receive_active flow =
    let open Bgp in
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 (local_id ());
      my_as = Asn (Int32.to_int (local_asn ()));
      options = [];
      hold_time = 180;
    } in
    write_msg flow (Bgp.Open o)
    >>= fun () ->
    read_flow_once flow 
    >>= fun () ->
    write_msg flow (Bgp.Keepalive)
    >>= fun () ->
    read_flow_once flow
    >>= fun () ->
    let _ =  write_keepalive flow in
    Lwt.join [
      read_flow_loop flow
    ]
  ;;
 
  let start s = 
    Bgp_flow.create_connection s (remote_id (), remote_port ())
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
  ;;
end

