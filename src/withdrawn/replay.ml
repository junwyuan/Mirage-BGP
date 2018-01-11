open Lwt.Infix
open Bgp
open Mrt
open Printf

module Prefix_set = Set.Make (String)

module Ip_gen = struct
  let default_seed = 
    Int32.shift_left 128_l 24
  
  let next seed = 
    let s = Int32.add seed 256_l in
    (Afi.IPv4 s, s)
end

(* Logging *)
let replay_log = Logs.Src.create "Replay" ~doc:"Logging for Replay"
module Replay_log = (val Logs.src_log replay_log : Logs.LOG)

module  Main (S: Mirage_stack_lwt.V4) = struct
  let remote_id () = Ipaddr.V4.of_string_exn "127.0.0.1"

  let local_id () =
    let open Relay in
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2.id
    | _ -> Relay.dev_relay2.id
  ;;

  let remote_port () = 
    let open Relay in
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2.port
    | _ -> Relay.dev_relay2.port
  ;;

  let local_asn () = 
    let open Relay in
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2.as_no
    | _ -> Relay.dev_relay2.as_no
  ;;

  let filename () = Key_gen.filename ();;

  let is_burst () = Key_gen.burst ();;

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
      | None -> Lwt.return "No msg"
      | Some v -> Lwt.return (Bgp.to_string v))
    | Error _ -> Lwt.return "TCP read error"
    | _ -> Lwt.return "Connection closed")
    >>= fun s -> 
    Replay_log.debug (fun m -> m "%s" s);
    Lwt.return_unit
  ;;

  let write_tcp_msg flow msg =
    Replay_log.debug (fun m -> m "Send msg: %s" (Bgp.to_string msg));
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

  let local_id_int32 = Ipaddr.V4.to_int32(Ipaddr.V4.of_string_exn "172.19.10.1")

  let modify_packet t = match t with
    | Open _ | Notification _ | Keepalive -> t
    | Update {withdrawn; path_attrs; nlri} ->
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
  let marker () = 
    let withdrawn = [] in
    let nlri = [(Afi.IPv4 (Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn "10.0.0.0")), 8)] in
    let flags = {
      optional=false;
      transitive=true;
      partial=false;
      extlen=false;
    } in
    let path_attrs = [
      (flags, Origin EGP);
      (flags, Next_hop local_id_int32);
      (flags, As_path [Bgp.Seq [local_asn (); 2_l; 3_l]])
    ] in
    Update {withdrawn; nlri; path_attrs}
  ;;

  let start_replay ?(is_quick=true) flow =
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
        match packet with
        | Update _ -> 
          incr npackets;
          let p_modified = modify_packet packet in
          write_tcp_msg flow p_modified
        | _ -> Lwt.return_unit
    in
    
    let rec replay ?(is_quick=true) prev prefix_set packets =
      if (!npackets >= total && not (total = 0)) then Lwt.return prefix_set
      else match packets () with
      | None -> 
        write_tcp_msg flow (marker ())
        >>= fun () ->
        Lwt.return prefix_set
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

  let rec plain_feed flow count seed =
    if count >= (Key_gen.total ()) then 
      write_tcp_msg flow (marker ())
      >>= fun () ->
      Lwt.return_unit
    else 
      let withdrawn = [] in
      let path_attrs = 
        let open Bgp in
        let flags = { optional=false; transitive=true; extlen=false; partial=false } in
        let origin = Origin EGP in
        let next_hop = Next_hop local_id_int32 in
        let as_path = As_path [Seq [4_l; 2_l; 3_l]] in
        List.map (fun pa -> (flags, pa)) [origin; next_hop; as_path]
      in
      let ip, n_seed = Ip_gen.next seed in
      let nlri = [(ip, 24)] in
      let u = { withdrawn; path_attrs; nlri } in
      write_tcp_msg flow (Bgp.Update u)
      >>= fun () ->
      plain_feed flow (count + 1) n_seed
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
    Lwt.join [plain_feed flow 0 Ip_gen.default_seed]
  ;;

  let start s =    
    S.TCPV4.create_connection (S.tcpv4 s) (remote_id (), remote_port ())
    >>= function
    | Ok flow -> 
      Replay_log.info (fun m -> m "Connect to remote");
      start_bgp_active flow
    | Error err -> 
      (match err with 
      | `Refused -> Replay_log.info (fun m -> m "Conn refused")
      | `Timeout -> Replay_log.info (fun m -> m "Conn timeout")
      | _ -> ());
      Lwt.return_unit
  ;;    
end


