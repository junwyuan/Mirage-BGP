open Lwt.Infix
open Config_parser
open Bgp


let mon_log = Logs.Src.create "Monitor" ~doc:"Monitor log"
module Mon_log = (val Logs.src_log mon_log : Logs.LOG)

let speaker1 = Logs.Src.create "Peer1" ~doc:"Speaker 1 log"
module Sp1_log = (val Logs.src_log speaker1 : Logs.LOG)

let speaker2 = Logs.Src.create "Peer2" ~doc:"Speaker 2 log"
module Sp2_log = (val Logs.src_log speaker2 : Logs.LOG)

module Main (S: Mirage_stack_lwt.V4) = struct
  module Bgp_flow = Bgp_io.Make(S)
  
  let default_open_msg (relay: Config_parser.relay) = 
    let o = {
      version = 4;
      local_id = relay.local_id;
      local_asn = relay.local_asn;
      options = [ Capability [Mp_ext (Afi.IP4, Safi.UNICAST)] ];
      hold_time = 180;
    } in
    Bgp.Open o
  ;;

  let fail msg = 
    Mon_log.err (fun m -> m "%s" msg);
    Lwt.fail_with msg
  ;;

  let create_session s peer (module Log : Logs.LOG) =
    Bgp_flow.create_connection s (peer.remote_id, peer.remote_port)
    >>= function
    | Error _ -> 
      Log.err (fun m -> m "Create_session: tcp connect fail");
      assert false
    | Ok flow ->
      Bgp_flow.write flow (default_open_msg peer)
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

  let close_session flow (module Log : Logs.LOG) = 
    Log.debug (fun m -> m "Send message: %s" (to_string (Notification Cease)));
    Bgp_flow.write flow (Bgp.Notification Bgp.Cease)
    >>= function
    | Error _ -> fail "tcp write fail"
    | Ok () ->
      Bgp_flow.close flow
  ;;

  let plain_feed ?(num_pfx_per_msg=500) ~total_num_msg ~seed ~flow ~path_attrs (module Log : Logs.LOG) =
    let rec loop count seed = 
      if count >= total_num_msg then Lwt.return seed
      else 
        let nlri, n_seed = Pfx_gen.gen seed num_pfx_per_msg in
        let msg = Update { withdrawn=[]; path_attrs; nlri } in

        Log.debug (fun m -> m "Feed %d: %s" count (Bgp.to_string msg));
        
        Bgp_flow.write flow msg
        >>= function
        | Error _ ->
          Log.err (fun m -> m "fail to write");
          Lwt.fail_with "fail to write"
        | Ok () -> 
          loop (count + 1) n_seed
    in
    loop 0 seed
  ;;


  let plain_feed_withdrawl ?(num_pfx_per_msg=500) ~total_num_msg ~seed ~flow (module Log : Logs.LOG) =
    let rec loop count seed = 
      if count >= total_num_msg then Lwt.return seed
      else
        let withdrawn, n_seed = Pfx_gen.gen seed num_pfx_per_msg in
        let msg = Update { withdrawn; path_attrs = []; nlri= [] } in

        Log.debug (fun m -> m "Feed %d: %s" count (Bgp.to_string msg));
        
        Bgp_flow.write flow msg
        >>= function
        | Error _ ->
          Log.err (fun m -> m "fail to write");
          Lwt.fail_with "fail to write"
        | Ok () -> 
          loop (count + 1) n_seed
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

  let read_loop_count_nlri flow total_num_pfx (module Log : Logs.LOG) =
    let rec loop count =
      match%lwt Bgp_flow.read flow with
      | Ok msg -> 
        (match msg with
        | Update { withdrawn; nlri; path_attrs } ->
          Log.debug (fun m -> m "Rec: %s" (Bgp.to_string msg));

          let new_count = count + List.length nlri in
          Log.debug (fun m -> m "Insert count %d" new_count);
          
          if new_count = total_num_pfx then Lwt.return_unit else loop new_count
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

  let read_loop_count_withdrawn flow total_num_pfx (module Log : Logs.LOG) =
    let rec loop count =
      match%lwt Bgp_flow.read flow with
      | Ok msg -> 
        (match msg with
        | Update { withdrawn; nlri; path_attrs } ->
          Log.debug (fun m -> m "Rec: %s" (Bgp.to_string msg));

          let new_count = count + List.length withdrawn in
          Log.debug (fun m -> m "Withdrawl count %d" new_count);
          
          if new_count = total_num_pfx then Lwt.return_unit else loop new_count
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

  let phased_insert ?(num_pfx_per_msg=500) ?(num_pfx_per_round=100000) ~phase_name ~total_num_pfx ~seed ~path_attrs ~flow1 ~flow2 (module Log1 : Logs.LOG) (module Log2 : Logs.LOG) =
    let num_stages = total_num_pfx / num_pfx_per_round in
    let rec loop stage_count seed time = 
      if stage_count = num_stages then begin
        Log1.info (fun m -> m "%s phase total: %.4fs" phase_name time);
        Lwt.return seed
      end 
      else begin
        (* Latency test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_nlri flow2 num_pfx_per_msg (module Log2) in
        (* Ping *)
        let start_time = Unix.gettimeofday () in
        
        plain_feed ~num_pfx_per_msg ~total_num_msg:1 ~flow:flow1 ~seed ~path_attrs (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop
        >>= fun () ->
        let latency = Unix.gettimeofday () -. start_time in

        OS.Time.sleep_ns (Duration.of_sec 1) 
        >>= fun () ->

        (* Throughput test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_nlri flow2 (num_pfx_per_round - num_pfx_per_msg) (module Log2) in
        (* start feeding speaker1 *)
        let start_time = Unix.gettimeofday () in
        plain_feed ~num_pfx_per_msg ~total_num_msg:(num_pfx_per_round / num_pfx_per_msg - 1) ~flow:flow1 ~seed:n_seed ~path_attrs (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop 
        >>= fun () ->
        let insert_time = Unix.gettimeofday () -. start_time in

        Mon_log.info (fun m -> m "%s phase stage %d, RIB size %d: latency: %.4f s, total: %.4f s" 
                            phase_name stage_count (stage_count * num_pfx_per_round) latency insert_time);
        
        (* Cool down *)
        OS.Time.sleep_ns (Duration.of_sec 1)
        >>= fun () ->

        loop (stage_count + 1) n_seed (time +. insert_time)
      end
    in
    loop 0 seed 0.
  ;;

  let phased_withdrawn ?(num_pfx_per_msg=500) ?(num_pfx_per_round=100000) ~total_num_pfx ~seed ~flow1 ~flow2 (module Log1 : Logs.LOG) (module Log2 : Logs.LOG) =
    let num_stages = total_num_pfx / num_pfx_per_round in
    let rec loop stage_count seed time = 
      if stage_count = num_stages then begin
        Log1.info (fun m -> m "Withdrawn phase total: %.4fs" time);
        Lwt.return ()
      end 
      else begin
        (* Latency test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_withdrawn flow2 num_pfx_per_msg (module Log2) in
        (* Ping *)
        let start_time = Unix.gettimeofday () in
        
        plain_feed_withdrawl ~num_pfx_per_msg:num_pfx_per_msg ~total_num_msg:1 ~flow:flow1 ~seed (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop
        >>= fun () ->
        let latency = Unix.gettimeofday () -. start_time in

        OS.Time.sleep_ns (Duration.of_sec 1) 
        >>= fun () ->

        (* Throughput test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_withdrawn flow2 (num_pfx_per_round - num_pfx_per_msg) (module Log2) in
        (* start feeding speaker1 *)
        let start_time = Unix.gettimeofday () in
        plain_feed_withdrawl ~total_num_msg:(num_pfx_per_round / num_pfx_per_msg - 1) ~num_pfx_per_msg ~flow:flow1 ~seed:n_seed (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop 
        >>= fun () ->
        let wd_time = Unix.gettimeofday () -. start_time in

        Mon_log.info (fun m -> m "Withdrawn phase stage %d, RIB size %d: latency: %.4f s, total: %.4f s" 
                              stage_count (stage_count * num_pfx_per_round) latency wd_time);
        
        (* Cool down *)
        OS.Time.sleep_ns (Duration.of_sec 1)
        >>= fun () ->

        loop (stage_count + 1) n_seed (time +. wd_time)
      end
    in
    loop 0 seed 0.
  ;;
  
  let start_perf_test ?(num_pfx_per_msg=500) ?(num_pfx_per_round=50000) ~s ~total_num_pfx ~seed ~(config:Config_parser.config) =
    let peer1 = List.nth config.relays 0 in
    let peer2 = List.nth config.relays 1 in

    (* connect to speaker1 *)
    create_session s peer1 (module Sp1_log)
    >>= fun (flow1, _) ->
    (* connect to speaker2 *)
    create_session s peer2 (module Sp2_log)
    >>= fun (flow2, _) ->

    Mon_log.info (fun m -> m "Connection up.");

    (* Keepalive loop *)
    let _ = write_keepalive_loop flow2 (module Sp2_log) in

    (* Wait through the start up mechanism *)
    OS.Time.sleep_ns (Duration.of_sec (Key_gen.start_time ()))
    >>= fun () ->

    (* Insert phase *)
    let path_attrs = 
      let origin = Origin EGP in
      let next_hop = Next_hop peer1.local_id in
      let as_path = As_path [Asn_seq [peer1.local_asn; 2000_l; 2001_l; 2002_l; 2003_l]] in
      [origin; next_hop; as_path]
    in
    phased_insert ~num_pfx_per_round ~num_pfx_per_msg ~seed:(Pfx_gen.default_seed) ~total_num_pfx ~flow1 ~flow2 
                  ~path_attrs (module Sp1_log : Logs.LOG) (module Sp2_log : Logs.LOG) ~phase_name:"Insert"
    >>= fun n_seed ->

    (* Unchange phase *)
    let unchange_phase () =
      let rloop = read_loop_count_nlri flow1 1 (module Sp1_log : Logs.LOG) in
      let path_attrs = 
        let origin = Origin EGP in
        let next_hop = Next_hop peer2.local_id in
        let as_path = As_path [Asn_seq [peer2.local_asn; 2000_l; 2001_l; 2002_l; 2003_l; 2004_l]] in
        [origin; next_hop; as_path]
      in
      let start_time = Unix.gettimeofday () in
      plain_feed  ~num_pfx_per_msg ~total_num_msg:(num_pfx_per_round / num_pfx_per_msg) ~seed:Pfx_gen.default_seed ~path_attrs
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
        Mon_log.info (fun m -> m "unchange phase stage 0, RIB size %d: %.4f s" total_num_pfx (Unix.gettimeofday () -. start_time));
        Lwt.return_unit
    in
    unchange_phase ()
    >>= fun () ->


    (* Replace phase *)
    let path_attrs = 
      let origin = Origin EGP in
      let next_hop = Next_hop peer1.local_id in
      let as_path = As_path [Asn_seq [peer1.local_asn; 2004_l; 2005_l; 2006_l]] in
      [origin; next_hop; as_path]
    in
    phased_insert ~num_pfx_per_round ~num_pfx_per_msg ~path_attrs
                  ~seed:(Pfx_gen.default_seed) ~total_num_pfx:(total_num_pfx/2) 
                  ~flow1 ~flow2 
                  (module Sp1_log : Logs.LOG) (module Sp2_log : Logs.LOG) 
                  ~phase_name:"replace"
    >>= fun _ ->

    (* Withdrawn phase *)
    phased_withdrawn  ~num_pfx_per_round ~num_pfx_per_msg  ~total_num_pfx:(total_num_pfx/2) 
                      ~seed:(Pfx_gen.default_seed) 
                      ~flow1 ~flow2
                      (module Sp1_log : Logs.LOG) (module Sp2_log : Logs.LOG)
    >>= fun () ->


    (* Link flap phase *)
    (* Speaker2 starts another listen *)
    let rec_rloop2 = read_loop_count_withdrawn flow2 (total_num_pfx / 2) (module Sp2_log) in
    
    (* Buffer time for installation and processing overrun *)
    let buffer_time = 5 in
    OS.Time.sleep_ns (Duration.of_sec buffer_time)
    >>= fun () ->

    (* Close flows *)
    let start_time = Unix.gettimeofday () in
    close_session flow1 (module Sp1_log)
    >>= fun () ->

    (* Speaker 2 wait for all withdrawl *)
    rec_rloop2
    >>= fun () ->
    Mon_log.info (fun m -> m "link flap phase stage 0, RIB size %d: %.4f s" 
                    (total_num_pfx) (Unix.gettimeofday () -. start_time));
    (* Clean up *)
    close_session flow2 (module Sp2_log)
    >>= fun () ->

    Lwt.return seed
  ;;

  
  let start s =
    let config = Config_parser.parse_from_file (Key_gen.config ()) in
    let total_num_pfx = 200000 in
    let seed = Pfx_gen.default_seed in
    Mon_log.info (fun m -> m "Test starts.");
    start_perf_test ~num_pfx_per_msg:500 ~num_pfx_per_round:50000 ~s ~total_num_pfx ~seed ~config
    >>= fun new_seed ->
    Mon_log.info (fun m -> m "Test finishes.");
    Lwt.return_unit
  ;;
end

