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
      | _ -> S.TCPV4.close flow >>= fun () ->  Lwt.return "Connection closed.") >>= fun s ->
      Rec_log.info (fun m -> m "%fs: %s" ((Unix.gettimeofday ()) -. start_time) s);
      Lwt.return_unit
  ;;

  let write_tcp_msg flow = fun buf ->
    S.TCPV4.write flow buf >>= function
    | Error _ ->
      Rec_log.debug (fun m -> m "Fail TCP write."); 
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
    write_tcp_msg flow (Bgp.gen_keepalive ()) >>=
    write_keepalive flow
  ;;

  let ip4_of_ints a b c d =
    Int32.of_int ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
  ;;
    
  let start_bgp flow : unit Lwt.t = 
    let o = {
      version=4;
      my_as=Bgp.Asn (Key_gen.asn ());
      hold_time=180;
      bgp_id = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn (Key_gen.id ()));
      options=[];
    } 
    in
    let o = Bgp.gen_open o in
    write_tcp_msg flow o >>=
    read_tcp_msg flow >>= fun () ->
    let k = Bgp.gen_keepalive () in
    write_tcp_msg flow k >>= fun () ->
    Lwt.join [read_loop flow (); write_keepalive flow ()]
  ;;

  let rec loop () = 
    OS.Time.sleep_ns (Duration.of_sec 30) 
    >>= fun () ->
    Rec_log.info (fun m -> m "30 seconds mark.");
    loop ()
  ;;

  let start_receive flow =
    read_tcp_msg flow ()
    >>= fun () ->
    let open Bgp in
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn (Key_gen.host ()));
      my_as = Asn 12345;
      options = [];
      hold_time = 180;
    } in
    write_tcp_msg flow (Bgp.gen_open o)
    >>= fun () ->
    read_tcp_msg flow ()
    >>= fun () ->
    write_tcp_msg flow (Bgp.gen_keepalive ())
    >>= fun () ->
    read_loop flow ()
  ;;

  let start s =
    let port = 50000 in
    let _host = Ipaddr.V4.of_string_exn (Key_gen.host ()) in

    S.listen_tcpv4 s ~port (fun flow -> start_receive flow);
    loop ()
  ;;    
end



