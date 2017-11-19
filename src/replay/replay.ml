open Lwt.Infix
open Bgp

module  Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4) = struct
    
  (* let rec writer flow c = fun () ->
    Time.sleep_ns (Duration.of_sec 1) >>=
    get_client_msg c >>= 
    write_tcp_msg flow c >>=
    writer flow c

  let rec reader flow c = fun () ->
    read_tcp_msg flow c () >>= fun msg ->
    C.log c msg >>= 
    reader flow c

  let start c s =
    let port = get_port () in
    let host = get_host () in
    let tcp = S.tcpv4 s in *)
    (* S.TCPV4.create_connection tcp (host, port) >>= function
    | Ok flow -> Lwt.join [reader flow c (); writer flow c ()]
    | Error _ -> Logs.debug (fun f -> f "connect fail."); assert false *)


  let ip4_of_ints a b c d =
    Int32.of_int ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
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
      C.log c s
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

  let start_bgp flow c : unit Lwt.t = 
    let o = {
      version=4;
      my_as=Bgp.Asn 64513;
      hold_time=180;
      bgp_id= ip4_of_ints 10 249 160 105;
      options=[]
    } 
    in
    let o = Bgp.gen_open o in
    write_tcp_msg flow c o >>=
    read_tcp_msg flow c >>= fun () ->
    let k = Bgp.gen_keepalive () in
    write_tcp_msg flow c k >>=
    read_loop flow c
  ;;


  let start c s =
    let port = 179 in
    let host = Ipaddr.V4.of_string_exn "10.249.185.116" in
    let tcp_s = S.tcpv4 s in
    S.TCPV4.create_connection tcp_s (host, port) >>= function
      | Ok flow -> start_bgp flow c
      | Error err -> failwith "Connection failure"
  ;;
end
