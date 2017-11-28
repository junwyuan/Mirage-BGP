open Lwt.Infix
open Printf
open Bgp


module  Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4) = struct
  let start_time = Unix.gettimeofday ();;
  let time f c =
    let t = Sys.time () in
    f () >>= fun () ->
    C.log c (sprintf "Execution time: %fs\n" (Sys.time() -. t))
  ;;
  
  let read_tcp_msg flow c = fun () -> 
    S.TCPV4.read flow >>= fun s -> 
      (match s with 
      | Ok (`Data buf) -> 
        (match Bgp.parse buf () with
        | None -> failwith "no msg"
        | Some v -> Lwt.return (Bgp.to_string v))
      | Error _ -> S.TCPV4.close flow >>= fun () -> Lwt.fail_with "read fail"
      | _ -> S.TCPV4.close flow >>= fun () -> Lwt.fail_with "No data") >>= fun s ->
      C.log c (sprintf "%fs: %s" ((Unix.gettimeofday ()) -. start_time) s)
  ;;

  let write_tcp_msg flow c = fun buf ->
    S.TCPV4.write flow buf >>= function
    | Error _ -> S.TCPV4.close flow >>= fun () -> Lwt.fail_with "write fail"
    | Ok _ -> Lwt.return_unit
  ;;

  let rec read_loop flow c = fun () ->
    read_tcp_msg flow c () >>=
    read_loop flow c
  ;;

  let rec write_keepalive flow c = fun () ->
    OS.Time.sleep_ns (Duration.of_sec 30) >>= fun () ->
    write_tcp_msg flow c (Bgp.gen_keepalive ()) >>=
    write_keepalive flow c
  ;;


  let ip4_of_ints a b c d =
    Int32.of_int ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
  ;;
    
  let start_bgp flow c : unit Lwt.t = 
    let o = {
      version=4;
      my_as=Bgp.Asn (Key_gen.asn ());
      hold_time=180;
      bgp_id = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn (Key_gen.id ()));
      options=[];
    } 
    in
    let o = Bgp.gen_open o in
    write_tcp_msg flow c o >>=
    read_tcp_msg flow c >>= fun () ->
    let k = Bgp.gen_keepalive () in
    write_tcp_msg flow c k >>= fun () ->
    Lwt.join [read_loop flow c (); write_keepalive flow c ()]
  ;;

  let start c s =
    let port = 179 in
    let host = Ipaddr.V4.of_string_exn (Key_gen.host ()) in
    let tcp_s = S.tcpv4 s in
    S.TCPV4.create_connection tcp_s (host, port) >>= function
      | Ok flow -> start_bgp flow c
      | Error err -> failwith "Connection failure"
  ;;
end



