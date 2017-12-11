open Lwt.Infix
open Printf
open Bgp

let rec_log = Logs.Src.create "receiver" ~doc:"Receiver log"
module Rec_log = (val Logs.src_log rec_log : Logs.LOG)

module  Main (S: Mirage_stack_lwt.V4) = struct
  let start_time = Unix.gettimeofday ();;
  let time f c =
    let t = Sys.time () in
    f () >>= fun () ->
    Rec_log.info (fun m -> m "Execution time: %fs\n" (Sys.time() -. t));
    Lwt.return_unit
  ;;
  
  let read_tcp_msg flow = fun () -> 
    S.TCPV4.read flow >>= fun s -> 
      (match s with 
      | Ok (`Data buf) -> 
        (match Bgp.parse buf () with
        | None -> failwith "no msg"
        | Some v -> Lwt.return (Bgp.to_string v))
      | Error _ -> S.TCPV4.close flow >>= fun () -> Lwt.return "Read fail"
      | _ -> S.TCPV4.close flow >>= fun () ->  Lwt.return "Connection closed.") 
      >>= fun s ->
      Rec_log.info (fun m -> m "%s" s);
      Lwt.return_unit
  ;;

  let write_tcp_msg flow = fun msg ->
    Rec_log.info (fun m -> m "send TCP message %s" (Bgp.to_string msg));
    S.TCPV4.write flow (Bgp.gen_msg msg) >>= function
    | Error _ ->
      Rec_log.debug (fun m -> m "fail to send TCP message."); 
      S.TCPV4.close flow 
    | Ok _ -> Lwt.return_unit
  ;;

  let rec read_loop flow = fun () ->
    read_tcp_msg flow () >>=
    read_loop flow
  ;;

  let rec write_keepalive flow = fun () ->
    OS.Time.sleep_ns (Duration.of_sec 30) 
    >>= fun () ->
    write_tcp_msg flow Bgp.Keepalive >>=
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
    let flags = {optional=false; transitive=false; partial=false; extlen=false} in
    let path_attrs = [
      flags, Origin IGP;
      flags, As_path [Set [2_l; 5_l; 3_l]; Seq [10_l; 20_l; 30_l]];
      flags, Next_hop (ip4_of_ints 192 168 1 253);
    ] in 
    let u = Update {withdrawn; path_attrs; nlri} in
    write_tcp_msg flow u
  ;;

  let rec loop () = 
    OS.Time.sleep_ns (Duration.of_sec 60) 
    >>= fun () ->
    loop ()
  ;;

  let start_receive flow =
    read_tcp_msg flow ()
    >>= fun () ->
    let open Bgp in
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn (Key_gen.remote_id ()));
      my_as = Asn (Key_gen.local_asn ());
      options = [];
      hold_time = 180;
    } in
    write_tcp_msg flow (Bgp.Open o)
    >>= fun () ->
    read_tcp_msg flow ()
    >>= fun () ->
    write_tcp_msg flow (Bgp.Keepalive)
    >>= fun () ->
    Lwt.join [read_loop flow (); write_keepalive flow (); write_update flow]
  ;;

  let start s =
    let port = Key_gen.local_port () in
    S.listen_tcpv4 s ~port (fun flow -> start_receive flow);

    let _ = 
      S.TCPV4.create_connection (S.tcpv4 s) (Ipaddr.V4.of_string_exn (Key_gen.remote_id ()), Key_gen.remote_port ())
      >>= function
      | Error _ -> Rec_log.info (fun m -> m "Can't connect to remote."); Lwt.return_unit
      | Ok flow -> start_receive flow
    in

    loop ()
  ;;
end



