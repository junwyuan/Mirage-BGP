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

  let relay1 () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay1
    | "xen" -> Relay.xen_relay1
    | "frr" -> Relay.frr_relay1
    | "xorp" -> Relay.xorp_relay1
    | "bird" -> Relay.bird_relay1
    | _ -> Relay.dev_relay1
  ;;
  

  let relay2 () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2
    | "xen" -> Relay.xen_relay2
    | "frr" -> Relay.frr_relay2
    | "xorp" -> Relay.xorp_relay2
    | "bird" -> Relay.bird_relay2
    | _ -> Relay.dev_relay2
  ;;

  let default_hold_time = 180
  
  let default_open_msg (relay: Relay.t) = 
    let o = {
      version = 4;
      local_id = relay.local_id;
      local_asn = relay.local_asn;
      options = [];
      hold_time = default_hold_time;
    } in
    Bgp.Open o
  ;;

  let fail msg = 
    Mon_log.err (fun m -> m "%s" msg);
    Lwt.fail_with msg
  ;;

  let create_session s relay (module Log : Logs.LOG) =
    let open Relay in
    Bgp_flow.create_connection s (relay.remote_id, relay.remote_port)
    >>= function
    | Error _ -> 
      Log.err (fun m -> m "Create_session: tcp connect fail");
      assert false
    | Ok flow ->
      Bgp_flow.write flow (default_open_msg relay)
      >>= function
      | Error _ -> 
        Log.err (fun m -> m "Create_session: write open fail");
        assert false
      | Ok () ->
        Bgp_flow.read flow
        >>= function
        | Error _ -> 
          Log.err (fun m -> m "Create_session: read open fail");
          assert false
        | Ok msg ->
          let open Bgp in
          match msg with
          | Keepalive | Notification _ | Update _ -> 
            Log.err (fun m -> m "Create_session: wrong msg type (NOT OPEN): %s" (to_string msg));
            assert false
          | Open o ->
            Bgp_flow.write flow Bgp.Keepalive
            >>= function
            | Error _ -> 
              Log.err (fun m -> m "Create_session: write keepalive fail");
              assert false
            | Ok () ->
              Bgp_flow.read flow
              >>= function
              | Error _ -> 
                Log.err (fun m -> m "Create_session: read keepalive fail");
                assert false
              | Ok msg ->
                match msg with
                | Keepalive ->
                  Lwt.return (flow, o)
                | _ -> 
                  Log.err (fun m -> m "wrong msg type: %s" (Bgp.to_string msg));
                  assert false
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
        | Notification _ ->
          Log.err (fun m -> m "Rec NOTIF: %s" (to_string msg));
          Lwt.return_unit
        | _ -> loop count)
      | Error err ->
        (match err with 
        | `Refused -> Log.err (fun m -> m "Read refused")
        | `Timeout -> Log.err (fun m -> m "Read timeout")
        | `Closed -> Log.err (fun m -> m "Connection closed when read.")
        | `PARSE_ERROR err -> Log.err (fun m -> m "%s" (parse_error_to_string err))
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
        | Notification _ ->
          Log.err (fun m -> m "Rec NOTIF: %s" (to_string msg));
          Lwt.return_unit
        | _ -> loop count)
      | Error err ->
        (match err with 
        | `Refused -> Log.err (fun m -> m "Read refused")
        | `Timeout -> Log.err (fun m -> m "Read timeout")
        | `Closed -> Log.err (fun m -> m "Connection closed when read.")
        | `PARSE_ERROR err -> Log.err (fun m -> m "%s" (parse_error_to_string err))
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
        Log.info (fun m -> m "write keepalive");
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
        (* Throughput test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_nlri flow2 stage_size (module Log2) in
        
        (* start feeding speaker1 *)
        let start_time = Unix.gettimeofday () in
        plain_feed ~total:stage_size ~flow:flow1 ~seed:seed ~path_attrs (module Log1)
        >>= fun n_seed ->
        
        (* Speaker2 checks on receiving all updates *)
        rec_rloop 
        >>= fun () ->
        
        let insert_time = Unix.gettimeofday () -. start_time in
        Log1.info (fun m -> m "%s phase stage %d, RIB size %d: %.4f s" 
                            phase_name stage_count (stage_count * stage_size) insert_time);
        
        (* Cool down *)
        OS.Time.sleep_ns (Duration.of_sec 1)
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

        OS.Time.sleep_ns (Duration.of_sec 1) 
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
    create_session s (relay1 ()) (module Sp1_log)
    >>= fun (flow1, _) ->
    (* connect to speaker2 *)
    create_session s (relay2 ()) (module Sp2_log)
    >>= fun (flow2, _) ->

    Mon_log.info (fun m -> m "Connection up.");

    (* Wait through the start up mechanism *)
    let min_ad_intvl = 
      match (Key_gen.speaker ()) with
      | "quagga" -> 5
      | _ -> 5
    in
    OS.Time.sleep_ns (Duration.of_sec min_ad_intvl)
    >>= fun () ->

    (* Double Insert phase *)
    let t1 () = 
      let path_attrs = 
        let origin = Origin EGP in
        let next_hop = Next_hop (relay1 ()).local_id in
        let as_path = As_path [Asn_seq [(relay1 ()).local_asn; 1003_l]] in
        [origin; next_hop; as_path]
      in
      phased_insert ~stage_size ~cluster_size 
                    ~seed:(Pfx_gen.default_seed) ~total:100000 ~flow1 ~flow2 ~path_attrs 
                    (module Sp1_log : Logs.LOG) (module Sp2_log : Logs.LOG) 
                    ~phase_name:"insert clash"
      >>= fun _ ->
      Lwt.return_unit
    in
    let t2 () = 
      let path_attrs = 
        let origin = Origin EGP in
        let next_hop = Next_hop (relay2 ()).local_id in
        let as_path = As_path [Asn_seq [(relay2 ()).local_asn; 1003_l]] in
        [origin; next_hop; as_path]
      in
      phased_insert ~stage_size ~cluster_size 
                    ~seed:(Pfx_gen.peek_next_n Pfx_gen.default_seed 100000) 
                    ~total:100000 ~flow1:flow2 ~flow2:flow1 
                    ~path_attrs (module Sp2_log : Logs.LOG) (module Sp1_log : Logs.LOG) 
                    ~phase_name:"insert clash"
      >>= fun _ ->
      Lwt.return_unit
    in
    Lwt.join [t1 (); t2 ()]
    >>= fun () ->


    close_session flow1
    >>= fun () ->
    close_session flow2
    >>= fun () ->
    Lwt.return seed
  ;;

  
  let start s =
    let test_sizes = [200000] in
    let rec loop seed = function
      | [] -> Lwt.return_unit
      | hd::tl -> 
        start_perf_test ~cluster_size:500 ~stage_size:50000 ~s ~total:hd ~seed 
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