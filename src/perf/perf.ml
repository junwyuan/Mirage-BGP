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
    | _ -> Relay.dev_relay1
  ;;

  let relay2 () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2
    | "host" -> Relay.host_relay2
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
            fail "Create_session: wrong msg type (not OPEN)"
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

  let plain_feed ?(cluster_size=500) ~num_msg ~seed ~flow ~relay (module Log : Logs.LOG) =
    let rec loop count seed = 
      if count = num_msg then Lwt.return_unit
      else 
        let path_attrs = 
          let origin = Origin EGP in
          let next_hop = Next_hop relay.id in
          let as_path = As_path [Asn_seq [relay.as_no; 1000_l; 1001_l; 1002_l]] in
          [origin; next_hop; as_path]
        in
        let nlri, n_seed = Pfx_gen.gen seed cluster_size in
        let msg = Update { withdrawn=[]; path_attrs; nlri } in

        Log.debug (fun m -> m "Feed pkg %d: %s" count (Bgp.to_string msg));
        
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
        | `Refused -> Log.debug (fun m -> m "Read refused")
        | `Timeout -> Log.debug (fun m -> m "Read timeout")
        | `Closed -> Log.debug (fun m -> m "Connection closed when read.")
        | `PARSE_ERROR err ->
          Log.debug (fun m -> m "%s" (parse_error_to_string err))
        | `Closed_by_local -> Log.debug (fun m -> m "Flow closed by local.")
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
        | `Refused -> Log.debug (fun m -> m "Read refused")
        | `Timeout -> Log.debug (fun m -> m "Read timeout")
        | `Closed -> Log.debug (fun m -> m "Connection closed when read.")
        | `PARSE_ERROR err -> Log.debug (fun m -> m "%s" (parse_error_to_string err))
        | `Closed_by_local -> Log.debug (fun m -> m "Flow closed by local")
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
        | `Refused -> Log.debug (fun m -> m "Read refused")
        | `Timeout -> Log.debug (fun m -> m "Read timeout")
        | `Closed -> Log.debug (fun m -> m "Connection closed when read.")
        | `PARSE_ERROR err -> Log.debug (fun m -> m "%s" (parse_error_to_string err))
        | `Closed_by_local -> Log.debug (fun m -> m "Flow closed by local")
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
        Log.debug (fun m -> m "Write keepalive failed. This may be expected.");
        Lwt.fail_with "Write keepalive failed. This may be expected." 
      | Ok () ->
        loop ()
    in
    loop ()
  ;;
  
  let start_perf_test ?(cluster_size=500) ~s ~total ~seed =
    (* connect to speaker1 *)
    let%lwt (flow1, _) = create_session s (relay1 ()) in
    (* connect to speaker2 *)
    let%lwt (flow2, _) = create_session s (relay2 ()) in

    let num_msg = total / cluster_size in

    let min_ad_intvl = 
      match (Key_gen.speaker ()) with
      | "quagga" -> 3
      | _ -> 3
    in

    (* Keepalive loop *)
    let _ = write_keepalive_loop flow1 in
    let _ = write_keepalive_loop flow2 in

    let marker_id = Pfx_gen.peek_next_n seed total in

    (* Listen speaker2 *)
    let rec_rloop = read_loop_count_nlri flow2 total (module Sp2_log) in

    (* Wait out minimal advertisement time *)
    let%lwt () = OS.Time.sleep_ns (Duration.of_sec min_ad_intvl) in
    
    let start_time = Unix.gettimeofday () in
    
    (* start feeding speaker1 *)
    let%lwt () = plain_feed ~num_msg ~flow:flow1 ~seed ~relay:(relay1 ()) (module Sp1_log) in

    (* Speaker2 checks on receiving all updates *)
    let%lwt () = rec_rloop in

    let insert_time = Unix.gettimeofday () -. start_time in
    
    (* Speaker2 starts another listen *)
    let rec_rloop2 = read_loop_count_withdrawn flow2 total (module Sp2_log) in
    
    (* Buffer time for installation and processing overrun *)
    let%lwt () = OS.Time.sleep_ns (Duration.of_sec 10) in

    (* Close flows *)
    let%lwt () = close_session flow1 in

    (* Speaker 2 wait for all withdrawl *)
    let%lwt () = rec_rloop2 in

    let withdrawn_time = Unix.gettimeofday () -. start_time -. insert_time -. 10. in
    
    Mon_log.info (fun m -> m "Test size: %d, insert time: %fs, withdrawn time: %fs, total %fs"
                  total insert_time withdrawn_time (insert_time +. withdrawn_time));
    
    (* Clean up *)
    let%lwt () = close_session flow2 in

    Lwt.return marker_id
  ;;


  let start s =
    let test_sizes = [500000] in
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