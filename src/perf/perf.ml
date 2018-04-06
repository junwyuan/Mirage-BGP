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
      hold_time = 720;
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
                  OS.Time.sleep_ns (Duration.of_ms 500) >>= fun () ->
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

  let plain_feed ~pfx_per_msg ~asn ~num_msg ~seed ~flow ~(peer:Config_parser.relay) ~path_attrs (module Log : Logs.LOG) =
    let rec loop count seed asn_count = 
      if count >= num_msg then Lwt.return (seed, asn_count)
      else 
        let nlri, n_seed = Pfx_gen.gen seed pfx_per_msg in

        let path_attrs = 
          let origin = Origin EGP in
          let next_hop = Next_hop peer.local_id in
          let as_path = As_path [Asn_seq [peer.local_asn; asn_count; 65001_l; 65002_l; 65003_l]] in
          [origin; next_hop; as_path]
        in

        let msg = Update { withdrawn=[]; path_attrs; nlri } in

        Log.debug (fun m -> m "Feed %d: %s" count (Bgp.to_string msg));

        let asn_count = if asn_count > 65000_l then 3000_l else Int32.add asn_count 1_l in
        
        Bgp_flow.write flow msg
        >>= function
        | Error _ ->
          Log.err (fun m -> m "fail to write");
          Lwt.fail_with "fail to write"
        | Ok () -> 
          loop (count + 1) n_seed asn_count
    in
    loop 0 seed asn
  ;;


  let plain_feed_withdrawl ~pfx_per_msg ~num_msg ~seed ~flow (module Log : Logs.LOG) =
    let rec loop count seed = 
      if count >= num_msg then Lwt.return seed
      else
        let withdrawn, n_seed = Pfx_gen.gen seed pfx_per_msg in
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
      let%lwt () = OS.Time.sleep_ns (Duration.of_sec 60) in

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

  (* let phased_insert ?(pfx_per_msg=500) ?(num_pfx_per_round=100000) ~phase_name ~total_num_pfx ~seed ~path_attrs ~peer ~flow1 ~flow2 (module Log1 : Logs.LOG) (module Log2 : Logs.LOG) =
    let num_stages = total_num_pfx / num_pfx_per_round in
    let rec loop stage_count seed time = 
      if stage_count = num_stages then begin
        Log1.info (fun m -> m "%s phase total: %.4fs" phase_name time);
        Lwt.return seed
      end 
      else begin
        (* Latency test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_nlri flow2 pfx_per_msg (module Log2) in
        (* Ping *)
        let start_time = Unix.gettimeofday () in
        
        plain_feed ~pfx_per_msg ~num_msg:1 ~flow:flow1 ~peer ~seed ~path_attrs (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop
        >>= fun () ->
        let latency = Unix.gettimeofday () -. start_time in

        OS.Time.sleep_ns (Duration.of_sec 1) 
        >>= fun () ->

        (* Throughput test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_nlri flow2 (num_pfx_per_round - pfx_per_msg) (module Log2) in
        (* start feeding speaker1 *)
        let start_time = Unix.gettimeofday () in
        plain_feed ~pfx_per_msg ~num_msg:(num_pfx_per_round / pfx_per_msg - 1) ~peer ~flow:flow1 ~seed:n_seed ~path_attrs (module Log1)
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

  let phased_withdrawn ?(pfx_per_msg=500) ?(num_pfx_per_round=100000) ~total_num_pfx ~seed ~flow1 ~flow2 (module Log1 : Logs.LOG) (module Log2 : Logs.LOG) =
    let num_stages = total_num_pfx / num_pfx_per_round in
    let rec loop stage_count seed time = 
      if stage_count = num_stages then begin
        Log1.info (fun m -> m "Withdrawn phase total: %.4fs" time);
        Lwt.return ()
      end 
      else begin
        (* Latency test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_withdrawn flow2 pfx_per_msg (module Log2) in
        (* Ping *)
        let start_time = Unix.gettimeofday () in
        
        plain_feed_withdrawl ~pfx_per_msg:pfx_per_msg ~num_msg:1 ~flow:flow1 ~seed (module Log1)
        >>= fun n_seed ->
        (* Speaker2 checks on receiving all updates *)
        rec_rloop
        >>= fun () ->
        let latency = Unix.gettimeofday () -. start_time in

        OS.Time.sleep_ns (Duration.of_sec 1) 
        >>= fun () ->

        (* Throughput test *)
        (* Listen speaker2 *)
        let rec_rloop = read_loop_count_withdrawn flow2 (num_pfx_per_round - pfx_per_msg) (module Log2) in
        (* start feeding speaker1 *)
        let start_time = Unix.gettimeofday () in
        plain_feed_withdrawl ~num_msg:(num_pfx_per_round / pfx_per_msg - 1) ~pfx_per_msg ~flow:flow1 ~seed:n_seed (module Log1)
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
  
  let start_perf_test ?(pfx_per_msg=500) ?(num_pfx_per_round=50000) ~s ~total_num_pfx ~seed ~(config:Config_parser.config) =
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
    phased_insert ~num_pfx_per_round ~pfx_per_msg ~peer:peer1 ~seed:(Pfx_gen.default_seed) ~total_num_pfx ~flow1 ~flow2 
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
      plain_feed  ~pfx_per_msg ~num_msg:(num_pfx_per_round / pfx_per_msg) ~peer:peer1 ~seed:Pfx_gen.default_seed ~path_attrs
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
    phased_insert ~num_pfx_per_round ~pfx_per_msg ~path_attrs
                  ~seed:(Pfx_gen.default_seed) ~total_num_pfx:(total_num_pfx/2) 
                  ~flow1 ~flow2 ~peer:peer1
                  (module Sp1_log : Logs.LOG) (module Sp2_log : Logs.LOG) 
                  ~phase_name:"replace"
    >>= fun _ ->

    (* Withdrawn phase *)
    phased_withdrawn  ~num_pfx_per_round ~pfx_per_msg  ~total_num_pfx:(total_num_pfx/2) 
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
  ;; *)

  module Math = struct
    let mean l = 
      let sum = List.fold_left (fun acc x -> acc +. x) 0. l in
      sum /. float_of_int (List.length l)
    ;;

    let std l =
      let mean = mean l in
      let sum_sq_err = List.fold_left (fun acc x -> acc +. (x -. mean) *. (x -. mean)) 0. l in
      sqrt (sum_sq_err /. float_of_int (List.length l))
    ;;
  end

  let insert_phased_throughput_test ~num_round ~round_gap ~msg_per_round ~pfx_per_msg ~socket ~(config:Config_parser.config) () =
    let peer1 = List.nth config.relays 0 in
    let peer2 = List.nth config.relays 1 in

    (* connect to speaker1 *)
    create_session socket peer1 (module Sp1_log)
    >>= fun (flow1, _) ->
    (* connect to speaker2 *)
    create_session socket peer2 (module Sp2_log)
    >>= fun (flow2, _) ->

    Mon_log.info (fun m -> m "Connection up.");

    (* Keepalive loop *)
    let _ = write_keepalive_loop flow2 (module Sp2_log) in

    (* Wait through the start up mechanism *)
    OS.Time.sleep_ns (Duration.of_sec (Key_gen.start_time ()))
    >>= fun () ->

    let path_attrs = 
      let origin = Origin EGP in
      let next_hop = Next_hop peer1.local_id in
      let as_path = As_path [Asn_seq [peer1.local_asn; 65000_l; 65001_l; 65002_l; 65003_l]] in
      [origin; next_hop; as_path]
    in

    let pfx_per_round = pfx_per_msg * msg_per_round in

    let rec loop round_count seed asn results = 
      if round_count = num_round then Lwt.return results
      else
        let rec repeat count results : float list Lwt.t =
          if count > 0 then 
            let rec_rloop = read_loop_count_nlri flow2 pfx_per_round (module Sp2_log) in
            let start_time = Unix.gettimeofday () in          
            plain_feed  ~flow:flow1 
                        ~pfx_per_msg 
                        ~num_msg:msg_per_round
                        ~path_attrs
                        ~seed
                        ~asn
                        ~peer: peer1
                        (module Sp1_log : Logs.LOG)
            >>= fun _ ->
            rec_rloop >>= fun () ->
            let end_time = Unix.gettimeofday () in
            let throughput = (float_of_int pfx_per_round) /. (end_time -. start_time) in
            if Key_gen.decompile () then
              Mon_log.info (fun m -> m "Phased insert throughput: RIB size %d, time: %.4fs, throughput: %.2f per second." 
                                      (round_count * round_gap) (end_time -. start_time) throughput);

            let wd_rloop = read_loop_count_withdrawn flow2 pfx_per_round (module Sp2_log) in
            plain_feed_withdrawl  ~pfx_per_msg
                                  ~flow:flow1
                                  ~num_msg: msg_per_round
                                  ~seed
                                  (module Sp1_log : Logs.LOG)
            >>= fun _ -> 
            wd_rloop >>= fun () ->

            (* OS.Time.sleep_ns (Duration.of_sec 3) >>= fun () -> *)
            
            repeat (count - 1) ((end_time -. start_time)::results)
          else Lwt.return results
        in
        repeat (Key_gen.repeat_times ()) [] >>= fun times ->

        let mean, std = Math.mean times, Math.std times in
        let throughput = (float_of_int pfx_per_round) /. mean in
        let err = throughput *. (std /. mean) in

        Mon_log.info (fun m -> m "%d, %f, %f, %f, %f" (round_count * round_gap) mean std throughput err);
        
        let rec_rloop = read_loop_count_nlri flow2 round_gap (module Sp2_log) in
        plain_feed  ~flow:flow1 
                    ~pfx_per_msg:1000
                    ~num_msg: (round_gap / 1000)
                    ~path_attrs
                    ~asn
                    ~seed:seed
                    ~peer:peer1
                    (module Sp1_log : Logs.LOG)
        >>= fun (pfx_seed, new_asn) ->
        rec_rloop >>= fun () ->

        OS.Time.sleep_ns (Duration.of_sec 3) >>= fun () ->
        
        loop (round_count + 1) pfx_seed new_asn (((round_count * round_gap), (mean, std, throughput, err))::results)
    in
    loop 0 Pfx_gen.default_seed 3000_l [] >>= fun results ->

    let data = List.map (fun (rib_size, (mean, std, throughput, err)) -> Printf.sprintf "%d, %f, %f, %f, %f" rib_size mean std throughput err) (List.rev results) in
    Mon_log.info (fun m -> m "Phased insert throughput: %s" (String.concat "\n" data));

    (* Close flows *)
    close_session flow1 (module Sp1_log)
    >>= fun () ->
    close_session flow2 (module Sp2_log)
    >>= fun () ->

    Lwt.return_unit
  ;;

  let next (x, y) =
    if y >= 65000 then (x+1, 2500)
    else (x, y+1)
  ;; 


  (* let insert_throughput_test ~num_msg ~pfx_per_msg ~socket ~(config:Config_parser.config) () =
    let peer1 = List.nth config.relays 0 in
    let peer2 = List.nth config.relays 1 in

    (* connect to speaker1 *)
    create_session socket peer1 (module Sp1_log)
    >>= fun (flow1, _) ->
    (* connect to speaker2 *)
    create_session socket peer2 (module Sp2_log)
    >>= fun (flow2, _) ->

    Mon_log.info (fun m -> m "Connection up.");

    (* Keepalive loop *)
    let _ = write_keepalive_loop flow2 (module Sp2_log) in

    (* Wait through the start up mechanism *)
    OS.Time.sleep_ns (Duration.of_sec (Key_gen.start_time ()))
    >>= fun () ->

    let total = pfx_per_msg * num_msg in

    let rec_rloop = read_loop_count_nlri flow2 total (module Sp2_log) in

    let start_time = Unix.gettimeofday () in

    plain_feed  ~flow:flow1 
                ~pfx_per_msg 
                ~num_msg
                ~path_attrs:[]
                ~peer:peer1
                ~seed:Pfx_gen.default_seed
                (module Sp1_log : Logs.LOG)
    >>= fun pfx_seed ->
    rec_rloop >>= fun () ->

    let end_time = Unix.gettimeofday () in
    
    let throughput = (float_of_int total) /. (end_time -. start_time) in
        
    if Key_gen.decompile () then
      Mon_log.info (fun m -> m "Insert throughput: test size %d, time: %.4fs, throughput: %.2f per second." 
                              total (end_time -. start_time) throughput);
    
    (* Close flows *)
    close_session flow1 (module Sp1_log)
    >>= fun () ->
    close_session flow2 (module Sp2_log)
    >>= fun () ->

    Lwt.return_unit
  ;; *)

  let rec run tests = 
    let interval = 15 in
    match tests with
    | test::other -> 
      test () >>= fun () ->
      if other <> [] then 
        OS.Time.sleep_ns (Duration.of_sec interval) >>= fun () ->
        run other
      else Lwt.return_unit
    | [] -> Lwt.return_unit
  ;;
  
  let start socket =
    let config = Config_parser.parse_from_file (Key_gen.config ()) in

    Mon_log.info (fun m -> m "Test starts.");
    let tests = [
      insert_phased_throughput_test ~num_round:10
                                    ~round_gap:200000
                                    ~pfx_per_msg:(Key_gen.pfx_per_msg ())
                                    ~msg_per_round:(Key_gen.msg_per_round ())
                                    ~socket
                                    ~config;
      (* insert_throughput_test  ~pfx_per_msg:(Key_gen.pfx_per_msg ())
                              ~num_msg:(Key_gen.msg_per_round ())
                              ~socket
                              ~config; *)
    ] in
    run tests >>= fun () ->

    Mon_log.info (fun m -> m "Test finishes.");
    Lwt.return_unit
  ;;
end

