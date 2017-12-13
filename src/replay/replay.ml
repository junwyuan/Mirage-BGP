open Lwt.Infix
open Bgp
open Mrt
open Printf

module Prefix_set = Set.Make (String)

(* Logging *)
let replay_log = Logs.Src.create "Replay" ~doc:"Logging for Replay"
module Replay_log = (val Logs.src_log replay_log : Logs.LOG)

module  Main (S: Mirage_stack_lwt.V4) = struct
  let remote_id () = 
    let raw = Key_gen.remote_id () in
    Ipaddr.V4.of_string_exn raw
  ;;

  let local_asn () = Int32.of_int (Key_gen.local_asn ())

  let local_id () = 
    let raw = Key_gen.local_id () in
    Ipaddr.V4.of_string_exn raw
  ;;

  let filename () = Key_gen.filename ();;

  let is_quick () = Key_gen.quick ();;

  let time f =
    let t = Unix.gettimeofday () in
    f () >>= fun () ->
    Replay_log.info (fun m -> m "Execution time: %fs" (Unix.gettimeofday () -. t));
    Lwt.return_unit
  ;;
  
  let read_tcp_msg flow =
    S.TCPV4.read flow 
    >>= fun s -> 
    (match s with 
    | Ok (`Data buf) -> 
      (match Bgp.parse buf () with
      | None -> failwith "no msg"
      | Some v -> Lwt.return (Bgp.to_string v))
    | Error _ -> S.TCPV4.close flow >>= fun () -> Lwt.fail_with "read fail"
    | _ -> S.TCPV4.close flow >>= fun () -> Lwt.fail_with "No data") 
    >>= fun s -> 
    Replay_log.info (fun m -> m "%s" s);
    Lwt.return_unit
  ;;

  let write_tcp_msg flow msg =
    Replay_log.info (fun m -> m "Send msg: %s" (Bgp.to_string msg));
    S.TCPV4.write flow (Bgp.gen_msg ~test:true msg)
    >>= function
    | Error _ -> 
      S.TCPV4.close flow
      >>= fun () -> 
      Lwt.fail_with "write fail"
    | Ok _ -> Lwt.return_unit
  ;;

  let rec read_loop flow =
    read_tcp_msg flow
    >>= fun () ->
    read_loop flow
  ;;

  let ip4_of_ints a b c d =
    Int32.of_int ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
  ;;  

  let modify_packet t = match t with
    | Open _ | Notification _ | Keepalive -> t
    | Update {withdrawn; path_attrs; nlri} ->
      let local_id_int32 = Ipaddr.V4.to_int32 (local_id ()) in
      let modify_attr (flags, pl) seen = 
        let modified = match pl with
          | Next_hop _ -> Next_hop local_id_int32
          | As4_path l_segment -> (match l_segment with
            | [] -> As4_path [Seq [local_asn ()]]
            | hd::tl -> (match hd with
              | Set l -> As4_path [Set (List.cons (local_asn ()) l)]
              | Seq l -> As4_path [Seq (List.cons (local_asn ()) l)]
            ))
          | As_path l_segment -> 
            (match l_segment with
            | [] -> As4_path [Seq [local_asn ()]]
            | hd::tl -> (match hd with
              | Set l -> As4_path [Set (List.cons (local_asn ()) l)]
              | Seq l -> As4_path [Seq (List.cons (local_asn ()) l)]
            ))
          | attr -> attr
        in List.cons (flags, modified) seen
      in

      let modified = List.fold_right modify_attr path_attrs [] in
      
      let contain_next_hop path_attrs = 
        List.exists (fun (_, pl) -> 
          match pl with Next_hop _ -> true | _ -> false
        ) path_attrs
      in

      let contain_as_path path_attrs = 
        List.exists (fun (_, pl) -> 
          match pl with As_path _ -> true | _ -> false
        ) path_attrs
      in 

      let contain_origin path_attrs = 
        List.exists (fun (_, pl) -> 
          match pl with Origin _ -> true | _ -> false
        ) path_attrs
      in 

      let flags = {
        optional=false;
        transitive=true;
        partial=false;
        extlen=false;
      } in
      
      let with_as_path = if not (contain_as_path modified) then
        let as_path = (flags, As_path []) in
        List.append modified [as_path]
      else modified in
      
      let with_next_hop = if not (contain_next_hop with_as_path) then
        let next_hop = (flags, Next_hop local_id_int32) in
        List.append with_as_path [next_hop]
      else with_as_path in

      let with_origin = if not (contain_origin with_next_hop) then
        let origin = (flags, Origin EGP) in
        List.append with_next_hop [origin]
      else with_next_hop in

      Update {
        withdrawn;
        path_attrs = with_origin;
        nlri;
      }
  ;;

  let update_prefix_set prefix_set = function
    | Open _ | Notification _ | Keepalive -> prefix_set
    | Update {withdrawn; path_attrs; nlri} ->
      let after_wd = List.fold_left (fun acc x -> Prefix_set.remove (Afi.prefix_to_string x) acc) prefix_set withdrawn in
      let after_nlri = List.fold_left (fun acc x -> Prefix_set.add (Afi.prefix_to_string x) acc) after_wd nlri in
      after_nlri
  ;;

  let start_replay ?(is_quick=true) flow () =
    (* global packet counter *)
    let npackets = ref 0 in
    let total = Key_gen.total () in
    
    (* open file, create buf *)
    let fn = filename () in
    
    let fd = Unix.(openfile fn [O_RDONLY] 0) in
    
    let buf =
      let ba = Bigarray.(Array1.map_file fd Bigarray.char c_layout false (-1)) in
      Cstruct.of_bigarray ba
    in
    
    Replay_log.info (fun m -> m "file length %d KB" ((Cstruct.len buf) / 1024));
    
    (* generate packet iterator *)
    let packets = Mrt.parse buf in

    (* recursively iterate over packet iterator, printing as we go *)
    let feed_packet time = function
      | None -> Lwt.return_unit
      | Some packet ->
        incr npackets;
        let msg = modify_packet packet in
        write_tcp_msg flow msg
    in
    
    let rec replay ?(is_quick=true) prev prefix_set packets =
      if (!npackets >= total && not (total = 0)) then Lwt.return prefix_set
      else match packets () with
      | None -> Lwt.return prefix_set
      | Some mrt -> (match mrt with
        | (header, Mrt.Bgp4mp bgp4mp) -> 
          let sec = Int32.to_int header.ts_sec in
          let uses = Int32.to_int header.ts_usec in
          let time = sec * 1000 * 1000 + uses in
          (match bgp4mp with
          | (_, Bgp4mp.Message_as4 bgp_t) -> (match bgp_t with 
            | Some v -> begin
              let updated_pfx_set = update_prefix_set prefix_set v in
              (if (time > prev) && (not is_quick) && (prev != 0) then 
                OS.Time.sleep_ns (Duration.of_us (time - prev)) 
                >>= fun () -> 
                feed_packet (Int64.of_int time) (Some v)
              else feed_packet (Int64.of_int time) (Some v)) 
              >>= fun () ->
              replay ~is_quick time updated_pfx_set packets
            end
            | None -> replay ~is_quick prev prefix_set packets)
          | _ -> replay ~is_quick prev prefix_set packets)
        | _ -> replay ~is_quick prev prefix_set packets)
    in
    replay ~is_quick 0 (Prefix_set.empty) packets
    
    >>= fun prefix_set ->
    Replay_log.info (fun m -> m "num packets %d, num prefixes %d" !npackets (List.length (Prefix_set.elements prefix_set)));
    Unix.close fd;
    Lwt.return_unit
  ;;

  let rec plain_feed ?(is_quick=true) flow i =
    let withdrawn = [] in
    let path_attrs = 
      let open Bgp in
      let flags = { optional=false; transitive=true; extlen=false; partial=false } in
      let origin = Origin EGP in
      let next_hop = Next_hop (Ipaddr.V4.to_int32 (local_id ())) in
      let as_path = As_path [Seq [1_l; 2_l; 3_l]] in
      List.map (fun pa -> (flags, pa)) [origin; next_hop; as_path]
    in
    let nlri =
      let s = Printf.sprintf "192.168.%d.%d" ((i lsr 8) land 0xff) (i land 0xff) in
      [(Afi.IPv4 (Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn s)), 16)]
    in
    let u = { withdrawn; path_attrs; nlri } in
    
    if (i > 256) then Lwt.return_unit
    else 
      write_tcp_msg flow (Bgp.Update u)
      >>= fun () ->
      plain_feed flow (i + 1) 
  ;;

  let start_bgp_passive flow : unit Lwt.t = 
    let o = {
      version=4;
      my_as=Bgp.Asn (Int32.to_int (local_asn ()));
      hold_time=180;
      bgp_id = Ipaddr.V4.to_int32 (local_id ());
      options=[]
    } 
    in
    read_tcp_msg flow
    >>= fun () ->
    write_tcp_msg flow (Bgp.Open o)
    >>= fun () ->
    read_tcp_msg flow 
    >>= fun () ->
    write_tcp_msg flow (Bgp.Keepalive) 
    >>= fun () ->
    Lwt.join [read_loop flow; time (start_replay ~is_quick:(is_quick ()) flow)]
  ;;

  let start_bgp_active flow : unit Lwt.t = 
    let o = {
      version=4;
      my_as=Bgp.Asn (Int32.to_int (local_asn ()));
      hold_time=180;
      bgp_id = Ipaddr.V4.to_int32 (local_id ());
      options=[]
    } 
    in
    write_tcp_msg flow (Bgp.Open o)
    >>= fun () ->
    read_tcp_msg flow 
    >>= fun () ->
    write_tcp_msg flow (Bgp.Keepalive) 
    >>= fun () ->
    read_tcp_msg flow
    >>= fun () ->
    Lwt.join [read_loop flow; time (start_replay ~is_quick:(is_quick ()) flow)]
  ;;

  let start s =    
    let callback flow = start_bgp_passive flow in
    S.listen_tcpv4 s (Key_gen.local_port ()) callback;
    
    let _ = 
      S.TCPV4.create_connection (S.tcpv4 s) (remote_id (), Key_gen.remote_port ())
      >>= function
      | Ok flow -> start_bgp_active flow
      | Error err -> 
        (match err with 
        | `Refused -> Replay_log.info (fun m -> m "Conn refused")
        | `Timeout -> Replay_log.info (fun m -> m "Conn timeout")
        | _ -> ());
        Lwt.return_unit
    in

    let rec loop () = 
      OS.Time.sleep_ns (Duration.of_sec 60)
      >>= fun () ->
      loop ()
    in

    loop ()
  ;;
end


