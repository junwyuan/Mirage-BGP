open Lwt.Infix
open Bgp
open Printf

module  Main (C: Mirage_console_lwt.S) (S: Mirage_stack_lwt.V4) = struct
  

  let ip4_of_ints a b c d =
    Int32.of_int ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
  ;;

  let bgp_id = ip4_of_ints 192 168 1 101
  
  let my_as = 64513_l

  let modify_packet t = match t with
    | Open _ | Notification _ | Keepalive -> t
    | Update {withdrawn; path_attrs; nlri} ->
      let modify_attr (flags, pl) seen = 
        let modified = match pl with
          | Next_hop _ -> Next_hop bgp_id
          | As4_path l_segment -> (match l_segment with
            | [] -> As4_path [Seq [my_as]]
            | hd::tl -> (match hd with
              | Set l -> As4_path [Set (List.cons my_as l)]
              | Seq l -> As4_path [Seq (List.cons my_as l)]
            ))
          | As_path l_segment -> 
            (match l_segment with
            | [] -> As4_path [Seq [my_as]]
            | hd::tl -> (match hd with
              | Set l -> As4_path [Set (List.cons my_as l)]
              | Seq l -> As4_path [Seq (List.cons my_as l)]
            ))
          | attr -> attr
        in List.cons (flags, modified) seen
      in

      let modified = List.fold_right modify_attr path_attrs [] in

      let flags = {
        optional=false;
        transitive=true;
        partial=false;
        extlen=false;
      } in

      let as_path = (flags, As_path []) in
      let pa = List.cons as_path modified in
      Update {
        withdrawn;
        path_attrs = pa;
        nlri;
      }
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

  let feed_update flow c =
    (* global packet counter *)
    let npackets = ref 0 in
  
    (* open file, create buf *)
    let fn = "data/updates.20171111.0005" in
    let fd = Unix.(openfile fn [O_RDONLY] 0) in
    let buf =
      let ba = Bigarray.(Array1.map_file fd Bigarray.char c_layout false (-1)) in
      Cstruct.of_bigarray ba
    in
    C.log c (sprintf "file length %d\n%!" (Cstruct.len buf)) >>= fun () ->
    
    let rec get_packet packets = match packets () with
      | None -> Lwt.return None
      | Some mrt -> 
        (match mrt with
        | (_, Mrt.Bgp4mp bgp4mp) -> (match bgp4mp with
          | (_, Bgp4mp.Message_as4 bgp_t) -> (match bgp_t with 
            | Some v -> Lwt.return (Some v)
            | None -> get_packet packets)
          | _ -> get_packet packets)
        | _ -> get_packet packets)
    in
      
    (* generate packet iterator *)
    let packets = Mrt.parse buf in

    (* recursively iterate over packet iterator, printing as we go *)
    let rec feed_packet () =
      get_packet packets >>= function
        | None -> Lwt.return_unit
        | Some packet ->
          incr npackets;
          C.log c (Bgp.to_string packet) >>= fun () ->
          let p2 = modify_packet packet in
          let buf = Bgp.gen_msg ~test:true p2 in
          let p = Bgp.parse_buffer_to_t buf |> function
            | Error e -> 
              (match e with
              | General e -> C.log c (Bgp.to_string (Bgp.Notification e))
              | _ -> Lwt.fail_with "Notification error")
            | Ok v -> C.log c (Bgp.to_string v)
          in p >>= fun () ->   
          Cstruct.hexdump buf;
          write_tcp_msg flow c buf >>= fun () ->
          if (!npackets = 10) then Lwt.return_unit else 
          OS.Time.sleep_ns (Duration.of_ms 500) >>=  
          feed_packet
    in
    feed_packet () >>= fun () ->

    (* done! *)
    C.log c (sprintf "num packets %d\n%!" !npackets) >>= fun () ->
    Lwt.return_unit
  ;;

  let start_bgp flow c : unit Lwt.t = 
    let o = {
      version=4;
      my_as=Bgp.Asn 64513;
      hold_time=180;
      bgp_id;
      options=[]
    } 
    in
    let o = Bgp.gen_open o in
    write_tcp_msg flow c o >>=
    read_tcp_msg flow c >>= fun () ->
    let k = Bgp.gen_keepalive () in
    write_tcp_msg flow c k >>= fun () ->
    Lwt.join [read_loop flow c (); feed_update flow c]
  ;;

  let start c s =
    let port = 179 in
    let host = Ipaddr.V4.of_string_exn "192.168.1.103" in
    let tcp_s = S.tcpv4 s in
    S.TCPV4.create_connection tcp_s (host, port) >>= function
      | Ok flow -> start_bgp flow c
      | Error err -> failwith "Connection failure"
  ;;
end
