open Lwt.Infix
open Bgp
open Relay
open Config_parser

let (>>>) a b = a >>= fun () -> b

let test_log = Logs.Src.create "Conf" ~doc:"Conformance tests log"
module Test_log = (val Logs.src_log test_log : Logs.LOG)

let speaker1 = Logs.Src.create "Speaker1" ~doc:"Speaker 1 log"
module Sp1_log = (val Logs.src_log speaker1 : Logs.LOG)

let speaker2 = Logs.Src.create "Speaker2" ~doc:"Speaker 2 log"
module Sp2_log = (val Logs.src_log speaker2 : Logs.LOG)

let speaker3 = Logs.Src.create "Speaker3" ~doc:"Speaker 3 log"
module Sp3_log = (val Logs.src_log speaker3 : Logs.LOG)

let speaker4 = Logs.Src.create "Speaker4" ~doc:"Speaker 4 log"
module Sp4_log = (val Logs.src_log speaker4 : Logs.LOG)

module Main (S: Mirage_stack_lwt.V4) = struct 
  module Bgp_flow = Bgp_io.Make(S)

  let default_hold_time = 180

  let default_open_msg relay = 
    let o = {
      version = 4;
      local_id = relay.local_id;
      local_asn = relay.local_asn;
      options = [];
      hold_time = default_hold_time;
    } in
    Bgp.Open o
  ;;

  let is_empty_update { withdrawn; path_attrs; nlri } = withdrawn = [] && nlri = []

  let fail test_name fail_reason = 
    Test_log.err (fun m -> m "Test %s fails: %s" test_name fail_reason);
    assert false
  ;;

  let pass test_name = 
    Test_log.info (fun m -> m "Pass test %s" test_name);
    Lwt.return_unit
  ;;

  let timeout time task test_name fail_reason =
    let timer = 
      OS.Time.sleep_ns (Duration.of_sec time)
      >>= fun () ->
      fail test_name fail_reason
    in
    Lwt.pick [timer; task ()]
  ;;

  let send_msg flow msg (module Log : Logs.LOG) =
    Bgp_flow.write flow msg
    >>= function
    | Error _ -> 
      Log.err (fun m -> m "Error when sending: %s" (to_string msg));
      assert false
    | Ok () -> 
      Log.debug (fun m -> m "Send message %s" (to_string msg));
      Lwt.return_unit
  ;;

  let send_buf flow buf (module Log : Logs.LOG) =
    S.TCPV4.write flow buf
    >>= function
    | Error _ -> assert false
    | Ok () -> Lwt.return_unit
  ;;

  let rec read_loop flow cond (module Log : Logs.LOG) =
    Bgp_flow.read flow
    >>= function
    | Error err ->
      let () = match err with
        | `Closed -> 
          Log.debug (fun m -> m "Connection closed when read.");
        | `Refused -> 
          Log.debug (fun m -> m "Read refused.");
        | `Timeout -> 
          Log.debug (fun m -> m "Read timeout.");
        | `PARSE_ERROR err -> begin
          match err with
          | Bgp.Parsing_error -> 
            Log.warn (fun m -> m "Message parsing error");
          | Bgp.Msg_fmt_error err -> 
            Log.warn (fun m -> m "Message format error")
          | Bgp.Notif_fmt_error _ -> 
            Log.err (fun m -> m "Got an notification message error");
        end
        | err -> Log.err (fun m -> m "Unknown")
      in
      assert false
    | Ok Update u -> 
      Log.debug (fun m -> m "Rec: %s" (to_string (Update u)));
      if is_empty_update u || not (cond (Update u)) then 
        read_loop flow cond (module Log : Logs.LOG)
      else 
        (* Condition satisfied *)
        Lwt.return_unit
    | Ok msg -> 
      Log.debug (fun m -> m "Rec: %s" (to_string msg));
      if cond msg then 
        (* Condition satisfied *)
        Lwt.return_unit
      else read_loop flow cond (module Log : Logs.LOG)
  ;;

  let read_update_loop flow cond (module Log : Logs.LOG) =
    let wrapped = function
      | Keepalive -> false
      | Update u -> cond u
      | msg -> 
        Log.err (fun m -> m "Expecting update, but rec: %s" (to_string msg));
        assert false
    in
    read_loop flow wrapped (module Log : Logs.LOG)
  ;;

  let create_session s peer (module Log : Logs.LOG) =
    Bgp_flow.create_connection s (peer.remote_id, peer.remote_port) >>= function
    | Error _ -> 
      Log.err (fun m -> m "Create_session: tcp connect fail");
      assert false
    | Ok flow ->
      Bgp_flow.write flow (default_open_msg peer) >>= function
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
    send_msg flow (Notification Cease) (module Log) >>= fun () ->
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
        | _ -> ()); 
        Lwt.return_unit
    in
    loop 0
  ;;



  let test_krt_change s config () =
    let test_name = "krt_change" in
    Test_log.info (fun m -> m "Start test: %s" test_name);

    let relay1 = List.nth config.relays 0 in
    let relay2 = List.nth config.relays 1 in
    
    create_session s relay1 (module Sp1_log)
    >>= fun (flow1, _) ->
    create_session s relay2 (module Sp2_log)
    >>= fun (flow2, _) ->

    let pfx1 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.24.0") in
    let pfx2 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.25.0") in
    let pfx3 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.26.0") in

    (* insert 1 prefix *)
    let nlri = [ pfx1; pfx2; pfx3; ] in
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ relay1.local_asn; 65001_l; 65002_l; 65003_l ] ];
      Next_hop relay1.local_id;
    ] in
    let update = { withdrawn = []; nlri; path_attrs } in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    let count = ref 0 in
    let cond { nlri; path_attrs; withdrawn } =
      count := !count + List.length nlri;
      !count = 3
    in
    read_update_loop flow2 cond (module Sp2_log)
    >>= fun () ->

    Test_log.info (fun m -> m "3 prefixes should be inserted. Please check krt manually.");
    OS.Time.sleep_ns (Duration.of_sec (Key_gen.wtime ())) >>= fun () ->

    (* Withdraw 1 prefix *)
    let update = { withdrawn = [ pfx3 ]; nlri = []; path_attrs = [] } in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    let count = ref 0 in
    let cond { nlri; path_attrs; withdrawn }  =
      count := !count + List.length withdrawn;
      !count = 1
    in
    read_update_loop flow2 cond (module Sp2_log)
    >>= fun () ->

    Test_log.info (fun m -> m "1 prefix should be withdrawn. Please check krt manually.");
    OS.Time.sleep_ns (Duration.of_sec (Key_gen.wtime ())) >>= fun () ->

    close_session flow1 (module Sp1_log)
    >>= fun () ->
    close_session flow2 (module Sp2_log)
    >>= fun () ->

    Test_log.info (fun m -> m "Link flap, 2 prefixes should be withdrawn. Please check manually.");
    OS.Time.sleep_ns (Duration.of_sec (Key_gen.wtime ())) >>= fun () ->

    pass test_name
  ;;

  let test_resolve s config () = 
    let test_name = "resolvability" in
    Test_log.info (fun m -> m "Start test: %s" test_name);

    let relay1 = List.nth config.relays 0 in
    let relay2 = List.nth config.relays 1 in
    
    create_session s relay1 (module Sp1_log)
    >>= fun (flow1, _) ->
    create_session s relay2 (module Sp2_log)
    >>= fun (flow2, _) ->


    let pfx1 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.24.0") in
    let pfx4 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.27.0") in

    (* Valid next hop *)
    let nlri = [ pfx1 ] in
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ relay1.local_asn; 65001_l; 65002_l; 65003_l ] ];
      Next_hop relay1.local_id;
    ] in
    let update = { withdrawn = []; nlri; path_attrs } in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    let t = fun () -> read_update_loop flow2 (fun u -> true) (module Sp2_log) in
    timeout 3 t test_name "Expect some update but does not get it" >>= fun () ->

    (* Invalid next hop *)
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ relay1.local_asn; 65001_l; 65002_l; 65003_l ] ];
      Next_hop (Ipaddr.V4.of_string_exn "170.0.3.5"); (* Unreachable next hop *)
    ] in
    let update = { withdrawn = []; nlri = [pfx4]; path_attrs } in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    let t = read_update_loop flow2 (fun u -> fail test_name "The route is invalid so expect no update.") (module Sp2_log) in
    let alarm = OS.Time.sleep_ns (Duration.of_sec 3) in
    Lwt.pick [alarm; t] >>= fun () ->

    close_session flow1 (module Sp1_log)
    >>= fun () ->
    close_session flow2 (module Sp2_log)
    >>= fun () ->

    pass test_name
  ;;

  let test_access_list s config () =
    let test_name = "access list" in
    Test_log.info (fun m -> m "Start test: %s" test_name);

    let relay1 = List.nth config.relays 0 in
    let relay2 = List.nth config.relays 1 in

    create_session s relay1 (module Sp1_log)
    >>= fun (flow1, _) ->
    create_session s relay2 (module Sp2_log)
    >>= fun (flow2, _) ->

    (* This prefix should be filtered out *)
    let pfx1 = Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "10.10.0.0") in
    let pfx2 = Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "10.11.0.0") in

    (* Invalid prefix *)
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ relay1.local_asn; 65001_l; 65002_l; 65003_l ] ];
      Next_hop relay1.local_id;
    ] in
    let update = { withdrawn = []; nlri = [ pfx1 ]; path_attrs } in
    send_msg flow1 (Update update) (module Sp1_log) >>= fun () ->

    let t = read_update_loop flow2 (fun u -> fail test_name "The route is invalid so expect no update.") (module Sp2_log) in
    let alarm = OS.Time.sleep_ns (Duration.of_sec 5) in
    Lwt.pick [alarm; t] >>= fun () ->    

    let update = { withdrawn = []; nlri = [ pfx2 ]; path_attrs } in
    send_msg flow1 (Update update) (module Sp1_log) >>= fun () ->

    let t = fun () -> read_update_loop flow2 (fun u -> true) (module Sp2_log) in
    timeout 3 t test_name "Expect some update but does not get it" >>= fun () ->

    close_session flow1 (module Sp1_log)
    >>= fun () ->
    close_session flow2 (module Sp2_log)
    >>= fun () ->

    pass test_name
  ;;

  let post_establish_test_base s config f =
    let test_name = "access list" in
    Test_log.info (fun m -> m "Start test: %s" test_name);

    let relay1 = List.nth config.relays 0 in
    let relay2 = List.nth config.relays 1 in

    create_session s relay1 (module Sp1_log)
    >>= fun (flow1, _) ->
    create_session s relay2 (module Sp2_log)
    >>= fun (flow2, _) ->

    f flow1 flow2 >>= fun () ->

    close_session flow1 (module Sp1_log)
    >>= fun () ->
    close_session flow2 (module Sp2_log)
    >>= fun () ->

    pass test_name
  ;;


  let test_weight_set s config () =
    let test_name = "weight set" in
    Test_log.info (fun m -> m "Start test: %s" test_name);

    let relay1 = List.nth config.relays 0 in
    let relay2 = List.nth config.relays 1 in

    create_session s relay1 (module Sp1_log)
    >>= fun (flow1, _) ->
    create_session s relay2 (module Sp2_log)
    >>= fun (flow2, _) ->

    let pfx1 = Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "20.20.0.0") in

    (* Speaker 2 sends a route for pfx1 *)
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ relay2.local_asn; 65001_l; 65002_l; 65003_l ] ];
      Next_hop relay2.local_id;
    ] in
    let update = { withdrawn = []; nlri = [ pfx1 ]; path_attrs } in
    send_msg flow2 (Update update) (module Sp2_log) >>= fun () ->

    read_update_loop flow1 (fun u -> true) (module Sp1_log) >>= fun () ->

    (* Speaker 1 sends another route to pfx1 *)
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ relay1.local_asn; 65001_l; 65002_l; 65003_l ] ];
      Next_hop relay1.local_id;
    ] in
    let update = { withdrawn = []; nlri = [ pfx1 ]; path_attrs } in
    send_msg flow1 (Update update) (module Sp1_log) >>= fun () ->

    read_update_loop flow2 (fun u -> true) (module Sp2_log) >>= fun () ->

    close_session flow1 (module Sp1_log)
    >>= fun () ->
    close_session flow2 (module Sp2_log)
    >>= fun () ->

    pass test_name
  ;;

  let test_ebgp_ibgp_transit s config () =
    let test_name = "ebgp/ibgp transit" in
    Test_log.info (fun m -> m "Start test: %s" test_name);

    let relay1 = List.nth config.relays 0 in
    let relay3 = List.nth config.relays 2 in
  
    create_session s relay1 (module Sp1_log)
    >>= fun (flow1, _) ->
    create_session s relay3 (module Sp3_log)
    >>= fun (flow3, _) ->

    let pfx1 = Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "50.10.0.0") in
    let pfx2 = Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "50.11.0.0") in

    (* eBGP sends to iBGP *)
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ relay1.local_asn; 65001_l; 65002_l; 65003_l ] ];
      Next_hop relay1.local_id;
    ] in
    let update = { withdrawn = []; nlri = [ pfx1 ]; path_attrs } in
    send_msg flow1 (Update update) (module Sp1_log) >>= fun () ->

    read_update_loop flow3 (fun u -> true) (module Sp3_log) >>= fun () ->

    (* iBGP send to eBGP *)
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ 65001_l; 65002_l; 65003_l ] ];
      Next_hop relay3.local_id;
      Local_pref 20_l;
    ] in
    let update = { withdrawn = []; nlri = [ pfx2 ]; path_attrs } in
    send_msg flow3 (Update update) (module Sp3_log) >>= fun () ->

    read_update_loop flow1 (fun u -> true) (module Sp1_log) >>= fun () ->

    close_session flow1 (module Sp1_log)
    >>= fun () ->
    close_session flow3 (module Sp3_log)
    >>= fun () ->

    pass test_name
  ;;

  let test_no_ibgp_transit s config () =
    let test_name = "no ibgp transit" in
    Test_log.info (fun m -> m "Start test: %s" test_name);

    let relay3 = List.nth config.relays 2 in
    let relay4 = List.nth config.relays 3 in

    create_session s relay3 (module Sp3_log)
    >>= fun (flow3, _) ->
    create_session s relay4 (module Sp4_log)
    >>= fun (flow4, _) ->

    let pfx1 = Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "50.10.0.0") in

    (* Send a message from iBGP to iBGP *)
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ 65001_l; 65002_l; 65003_l ] ];
      Next_hop relay3.local_id;
    ] in
    let update = { withdrawn = []; nlri = [ pfx1 ]; path_attrs } in
    send_msg flow3 (Update update) (module Sp3_log) >>= fun () ->

    let t = read_update_loop flow4 (fun u -> fail test_name "There should be no update.") (module Sp4_log) in
    let alarm = OS.Time.sleep_ns (Duration.of_sec 5) in
    Lwt.pick [t; alarm] >>= fun () ->

    close_session flow3 (module Sp3_log)
    >>= fun () ->
    close_session flow4 (module Sp4_log)
    >>= fun () ->

    pass test_name
  ;;


  let rec run tests = 
    let interval = 10 in
    match tests with
    | test::other -> 
      test ()
      >>= fun () ->
      OS.Time.sleep_ns (Duration.of_sec interval)
      >>= fun () ->
      run other
    | [] -> Lwt.return_unit
  ;;

  

  let start s =
    (* Enable debug mode *)
    Printexc.record_backtrace true;

    let config = Config_parser.parse_from_file (Key_gen.config ()) in

    Test_log.info (fun m -> m "Tests starts.");

    let tests = [
      test_krt_change s config;
      test_resolve s config;
      test_access_list s config;
      test_weight_set s config;
      test_ebgp_ibgp_transit s config;
      test_no_ibgp_transit s config;
    ] in
    run tests >>= fun () ->

    Test_log.info (fun m -> m "Test finishes.");
    Lwt.return_unit
  ;;
end