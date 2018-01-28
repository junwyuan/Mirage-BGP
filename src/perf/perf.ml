open Lwt.Infix
open Relay
open Bgp


let mon_log = Logs.Src.create "Monitor" ~doc:"Monitor log"
module Mon_log = (val Logs.src_log mon_log : Logs.LOG)

let speaker1 = Logs.Src.create "Speaker1" ~doc:"Speaker 1 log"
module Sp1_log = (val Logs.src_log speaker1 : Logs.LOG)

let speaker2 = Logs.Src.create "Speaker2" ~doc:"Speaker 2 log"
module Sp2_log = (val Logs.src_log speaker2 : Logs.LOG)

module Main (S: Mirage_stack_lwt.V4) = struct
  module Bgp_flow = Bgp_io.Make(S)

  let relay_id () = Ipaddr.V4.of_string_exn "127.0.0.1"

  let relay1 () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay1
    | "host" -> Relay.host_relay1
    | "frr" -> Relay.frr_relay1
    | _ -> Relay.dev_relay1
  ;;

  let relay2 () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2
    | "host" -> Relay.host_relay2
    | "frr" -> Relay.frr_relay2
    | _ -> Relay.dev_relay2
  ;;


  let default_hold_time = 180
  
  let default_open_msg relay = 
    let o = {
      version = 4;
      local_id = relay.id;
      local_asn = relay.as_no;
      options = [];
      hold_time = default_hold_time;
    } in
    Bgp.Open o
  ;;

  let fail msg = 
    Mon_log.err (fun m -> m "%s" msg);
    Lwt.fail_with msg
  ;;

  let create_session s relay =
    let open Relay in
    Bgp_flow.create_connection s (relay_id (), relay.port)
    >>= function
    | Error _ -> fail "Create_session: tcp connect fail"
    | Ok flow ->
      Bgp_flow.write flow (default_open_msg relay)
      >>= function
      | Error _ -> fail "Create_session: write open fail"
      | Ok () ->
        Bgp_flow.read flow
        >>= function
        | Error _ -> fail "Create_session: read open fail"
        | Ok msg ->
          let open Bgp in
          match msg with
          | Keepalive | Notification _ | Update _ -> 
            fail (Printf.sprintf "Create_session: wrong msg type (NOT OPEN): %s" (to_string msg))
          | Open o ->
            Bgp_flow.write flow Bgp.Keepalive
            >>= function
            | Error _ -> fail "Create_session: tcp write keepalive fail"
            | Ok () ->
              Bgp_flow.read flow
              >>= function
              | Error _ -> fail "Create_session: tcp read keepalive fail"
              | Ok msg ->
                match msg with
                | Keepalive ->
                  Lwt.return (flow, o)
                | _ -> fail (Printf.sprintf "wrong msg type: %s" (Bgp.to_string msg))
  ;;

  let close_session flow = 
    Bgp_flow.write flow (Bgp.Notification Bgp.Cease)
    >>= function
    | Error _ -> fail "tcp write fail"
    | Ok () ->
      Bgp_flow.close flow
  ;;

  let plain_feed ?(cluster_size=500) ~total ~seed ~flow ~path_attrs (module Log : Logs.LOG) =
    let rec loop count seed = 
      if count = total then Lwt.return seed
      else 
        let nlri, n_seed = Pfx_gen.gen seed cluster_size in
        let msg = Update { withdrawn=[]; path_attrs; nlri } in

        Log.debug (fun m -> m "Feed %d: %s" count (Bgp.to_string msg));
        
        Bgp_flow.write flow msg
        >>= function
        | Error _ ->
          Log.err (fun m -> m "fail to write");
          Lwt.fail_with "fail to write"
        | Ok () -> 
          loop (count + cluster_size) n_seed
    in
    loop 0 seed
  ;;


  let plain_feed_withdrawl ?(cluster_size=500) ~total ~seed ~flow (module Log : Logs.LOG) =
    let rec loop count seed = 
      if count = total then Lwt.return seed
      else
        let withdrawn, n_seed = Pfx_gen.gen seed cluster_size in
        let msg = Update { withdrawn; path_attrs = []; nlri= [] } in

        Log.debug (fun m -> m "Feed %d: %s" count (Bgp.to_string msg));
        
        Bgp_flow.write flow msg
        >>= function
        | Error _ ->
          Log.err (fun m -> m "fail to write");
          Lwt.fail_with "fail to write"
        | Ok () -> 
          loop (count + cluster_size) n_seed
    in
    loop 0 seed
  ;;

  let rec read_loop flow (module Log : Logs.LOG) =
    let rec loop () =
      match%lwt Bgp_flow.read flow with
      | Ok msg -> 
        Log.debug (fun m -> m "Rec: %s" (Bgp.to_string msg));
        loop ()
      | Error err ->
        (match err with 
        | `Refused -> Log.err (fun m -> m "Read refused")
        | `Timeout -> Log.err (fun m -> m "Read timeout")
        | `Closed -> Log.err (fun m -> m "Connection closed when read.")
        | `PARSE_ERROR err ->
          Log.err (fun m -> m "%s" (parse_error_to_string err))
        | `Closed_by_local -> Log.err (fun m -> m "Flow closed by local.")
        | _ -> ()); 
        Lwt.return_unit
    in
    loop ()
  ;;

  let read_loop_count_nlri flow total (module Log : Logs.LOG) =
    let rec loop count =
      match%lwt Bgp_flow.read flow with
      | Ok msg -> 
        (match msg with
        | Update { withdrawn; nlri; path_attrs } ->
          Log.debug (fun m -> m "Rec: %s" (Bgp.to_string msg));

          let new_count = count + List.length nlri in
          Log.debug (fun m -> m "Insert count %d" new_count);
          
          if new_count = total then Lwt.return_unit else loop new_count
        | _ -> loop count)
      | Error err ->
        (match err with 
        | `Refused -> Log.err (fun m -> m "Read refused")
        | `Timeout -> Log.err (fun m -> m "Read timeout")
        | `Closed -> Log.err (fun m -> m "Connection closed when read.")
        | `PARSE_ERROR err -> Log.err (fun m -> m "%s" (parse_error_to_string err))
        | `Closed_by_local -> Log.err (fun m -> m "Flow closed by local")
        | _ -> ()); 
        Lwt.return_unit
    in
    loop 0
  ;;

  let read_loop_count_withdrawn flow total (module Log : Logs.LOG) =
    let rec loop count =
      match%lwt Bgp_flow.read flow with
      | Ok msg -> 
        (match msg with
        | Update { withdrawn; nlri; path_attrs } ->
          Log.debug (fun m -> m "Rec: %s" (Bgp.to_string msg));

          let new_count = count + List.length withdrawn in
          Log.debug (fun m -> m "Withdrawl count %d" new_count);
          
          if new_count = total then Lwt.return_unit else loop new_count
        | _ -> loop count)
      | Error err ->
        (match err with 
        | `Refused -> Log.err (fun m -> m "Read refused")
        | `Timeout -> Log.err (fun m -> m "Read timeout")
        | `Closed -> Log.err (fun m -> m "Connection closed when read.")
        | `PARSE_ERROR err -> Log.err (fun m -> m "%s" (parse_error_to_string err))
        | `Closed_by_local -> Log.err (fun m -> m "Flow closed by local")
        | _ -> ());
        Lwt.return_unit
    in
    loop 0
  ;;

  let rec write_keepalive_loop flow (module Log : Logs.LOG) =
    let rec loop () = 
      let%lwt () = OS.Time.sleep_ns (Duration.of_sec 30) in

      match%lwt Bgp_flow.write flow Bgp.Keepalive with
      | Error _ ->
        Log.err (fun m -> m "Write keepalive failed. This may be expected.");
        Lwt.fail_with "Write keepalive failed. This may be expected." 
      | Ok () ->
        loop ()
    in
    loop ()
  ;;

  let phased_insert ?(cluster_size=500) ?(stage_size=100000) ~phase_name ~total ~seed ~path_attrs ~flow1 ~flow2 (module Log1 : Logs.LOG) (module Log2 : Logs.LOG) =
    let num_stages = total / stage_size in
    let rec loop stage_count seed time = 
      if stage_count = num_stages then begin
        Log1.info (fun m -> m "%s phase total: %.4fs" phase_name time);
        Lwt.return seed
      end 
      else begin
        (* Latency test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_nlri flow2 cluster_size (module Log2) in
        (* Ping *)
        let start_time = Unix.gettimeofday () in
        
        plain_feed ~total:cluster_size ~flow:flow1 ~seed ~path_attrs (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop
        >>= fun () ->
        let latency = Unix.gettimeofday () -. start_time in

        OS.Time.sleep_ns (Duration.of_ms 1000) 
        >>= fun () ->

        (* Throughput test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_nlri flow2 (stage_size - cluster_size) (module Log2) in
        (* start feeding speaker1 *)
        let start_time = Unix.gettimeofday () in
        plain_feed ~total:(stage_size - cluster_size) ~flow:flow1 ~seed:n_seed ~path_attrs (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop 
        >>= fun () ->
        let insert_time = Unix.gettimeofday () -. start_time in

        Log1.info (fun m -> m "%s phase stage %d, RIB size %d: latency: %.4fs, total: %.4fs" 
                            phase_name stage_count (stage_count * stage_size) latency insert_time);
        
        (* Cool down *)
        OS.Time.sleep_ns (Duration.of_ms 2)
        >>= fun () ->

        loop (stage_count + 1) n_seed (time +. insert_time)
      end
    in
    loop 0 seed 0.
  ;;

  let phased_withdrawn ?(cluster_size=500) ?(stage_size=100000) ~total ~seed ~flow1 ~flow2 (module Log1 : Logs.LOG) (module Log2 : Logs.LOG) =
    let num_stages = total / stage_size in
    let rec loop stage_count seed time = 
      if stage_count = num_stages then begin
        Log1.info (fun m -> m "Withdrawn phase total: %.4fs" time);
        Lwt.return ()
      end 
      else begin
        (* Latency test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_withdrawn flow2 cluster_size (module Log2) in
        (* Ping *)
        let start_time = Unix.gettimeofday () in
        
        plain_feed_withdrawl ~total:cluster_size ~flow:flow1 ~seed (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop
        >>= fun () ->
        let latency = Unix.gettimeofday () -. start_time in

        OS.Time.sleep_ns (Duration.of_ms 1000) 
        >>= fun () ->

        (* Throughput test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_withdrawn flow2 (stage_size - cluster_size) (module Log2) in
        (* start feeding speaker1 *)
        let start_time = Unix.gettimeofday () in
        plain_feed_withdrawl ~total:(stage_size - cluster_size) ~flow:flow1 ~seed:n_seed (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop 
        >>= fun () ->
        let wd_time = Unix.gettimeofday () -. start_time in

        Log1.info (fun m -> m "Withdrawn phase stage %d, RIB size %d: latency: %.4fs, total: %.4fs" 
                              stage_count (stage_count * stage_size) latency wd_time);
        
        (* Cool down *)
        OS.Time.sleep_ns (Duration.of_sec 1)
        >>= fun () ->

        loop (stage_count + 1) n_seed (time +. wd_time)
      end
    in
    loop 0 seed 0.
  ;;
  
  let start_perf_test ?(cluster_size=500) ?(stage_size=50000) ~s ~total ~seed =
    (* connect to speaker1 *)
    create_session s (relay1 ())
    >>= fun (flow1, _) ->
    (* connect to speaker2 *)
    create_session s (relay2 ())
    >>= fun (flow2, _) ->

    (* Keepalive loop *)
    let _ = write_keepalive_loop flow1 in
    let _ = write_keepalive_loop flow2 in

    (* Wait through the start up mechanism *)
    let min_ad_intvl = 
      match (Key_gen.speaker ()) with
      | "quagga" -> 3
      | _ -> 3
    in
    OS.Time.sleep_ns (Duration.of_sec min_ad_intvl)
    >>= fun () ->

    (* Insert phase *)
    let path_attrs = 
      let origin = Origin EGP in
      let next_hop = Next_hop (relay1 ()).id in
      let as_path = As_path [Asn_seq [(relay1 ()).as_no; 1000_l; 1001_l; 1002_l; 1003_l]] in
      [origin; next_hop; as_path]
    in
    phased_insert ~stage_size ~cluster_size ~seed:(Pfx_gen.default_seed) ~total ~flow1 ~flow2 
                  ~path_attrs (module Sp1_log : Logs.LOG) (module Sp2_log : Logs.LOG) ~phase_name:"Insert"
    >>= fun n_seed ->

    (* Unchange phase *)
    let unchange_phase () =
      let rloop = read_loop_count_nlri flow1 1 (module Sp1_log : Logs.LOG) in
      let path_attrs = 
        let origin = Origin EGP in
        let next_hop = Next_hop (relay2 ()).id in
        let as_path = As_path [Asn_seq [(relay2 ()).as_no; 1000_l; 1001_l; 1002_l; 1003_l; 1004_l]] in
        [origin; next_hop; as_path]
      in
      let start_time = Unix.gettimeofday () in
      plain_feed  ~cluster_size ~total:stage_size ~seed:Pfx_gen.default_seed ~path_attrs
                  ~flow:flow2 (module Sp2_log : Logs.LOG)
      >>= fun _ ->
      let update = {
        withdrawn = [];
        path_attrs;
        nlri = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.56.42.0") ];
      } in
      Bgp_flow.write flow2 (Update update)
      >>= function
      | Error _ -> assert false
      | Ok () -> 
        rloop
        >>= fun () ->
        Mon_log.info (fun m -> m "unchange phase stage 0, RIB size %d: %.4fs" total (Unix.gettimeofday () -. start_time));
        Lwt.return_unit
    in
    unchange_phase ()
    >>= fun () ->


    (* Replace phase *)
    let path_attrs = 
      let origin = Origin EGP in
      let next_hop = Next_hop (relay1 ()).id in
      let as_path = As_path [Asn_seq [(relay1 ()).as_no; 1004_l; 1005_l; 1006_l]] in
      [origin; next_hop; as_path]
    in
    phased_insert ~stage_size ~cluster_size ~path_attrs
                  ~seed:(Pfx_gen.default_seed) ~total:200000 
                  ~flow1 ~flow2 
                  (module Sp1_log : Logs.LOG) (module Sp2_log : Logs.LOG) 
                  ~phase_name:"replace"
    >>= fun _ ->

    (* Withdrawn phase *)
    phased_withdrawn  ~stage_size ~cluster_size  ~total:200000 
                      ~seed:(Pfx_gen.default_seed) 
                      ~flow1 ~flow2
                      (module Sp1_log : Logs.LOG) (module Sp2_log : Logs.LOG)
    >>= fun () ->

    (* Double Insert phase *)
    let t1 () = 
      let path_attrs = 
        let origin = Origin EGP in
        let next_hop = Next_hop (relay1 ()).id in
        let as_path = As_path [Asn_seq [(relay1 ()).as_no; 1003_l]] in
        [origin; next_hop; as_path]
      in
      phased_insert ~stage_size ~cluster_size 
                    ~seed:(Pfx_gen.default_seed) ~total:200000 ~flow1 ~flow2 ~path_attrs 
                    (module Sp1_log : Logs.LOG) (module Sp2_log : Logs.LOG) 
                    ~phase_name:"insert clash"
      >>= fun _ ->
      Lwt.return_unit
    in
    let t2 () = 
      let path_attrs = 
        let origin = Origin EGP in
        let next_hop = Next_hop (relay2 ()).id in
        let as_path = As_path [Asn_seq [(relay2 ()).as_no; 1003_l]] in
        [origin; next_hop; as_path]
      in
      phased_insert ~stage_size ~cluster_size 
                    ~seed:(Pfx_gen.peek_next_n Pfx_gen.default_seed 200000) 
                    ~total:200000 ~flow1:flow2 ~flow2:flow1 
                    ~path_attrs (module Sp2_log : Logs.LOG) (module Sp1_log : Logs.LOG) 
                    ~phase_name:"insert clash"
      >>= fun _ ->
      Lwt.return_unit
    in
    (* Lwt.join [t1 (); t2 ()]
    >>= fun () -> *)


    (* Link flap phase *)
    (* Speaker2 starts another listen *)
    let rec_rloop2 = read_loop_count_withdrawn flow2 (total - 200000) (module Sp2_log) in
    
    (* Buffer time for installation and processing overrun *)
    let buffer_time = 5 in
    OS.Time.sleep_ns (Duration.of_sec buffer_time)
    >>= fun () ->

    (* Close flows *)
    let start_time = Unix.gettimeofday () in
    close_session flow1
    >>= fun () ->

    (* Speaker 2 wait for all withdrawl *)
    rec_rloop2
    >>= fun () ->
    Mon_log.info (fun m -> m "link flap phase stage 0, RIB size %d: %.4fs" 
                    (total - 200000) (Unix.gettimeofday () -. start_time));
    (* Clean up *)
    close_session flow2
    >>= fun () ->

    Lwt.return seed
  ;;


  let start s =
    let test_sizes = [400000] in
    let rec loop seed = function
      | [] -> Lwt.return_unit
      | hd::tl -> 
        start_perf_test s hd seed 
        >>= fun new_seed ->
        (* Allow router to recover *)
        let interval = 
          match (Key_gen.speaker ()) with
          | "quagga" -> 30
          | _ -> 30
        in
        OS.Time.sleep_ns (Duration.of_sec interval)
        >>= fun () ->
        loop new_seed tl
    in
    loop (Pfx_gen.default_seed) test_sizes
  ;;
end