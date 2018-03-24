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

  let timeout test_name time task =
    let timer = 
      OS.Time.sleep_ns (Duration.of_sec time)
      >>= fun () ->
      fail test_name "timeout"
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

  let close_session flow = 
    Bgp_flow.write flow (Notification Cease)
    >>= function
    | Error _ -> fail "unknown" "fail to write"
    | Ok () -> Bgp_flow.close flow
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

  

 

  let start_test s config =
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

    Test_log.info (fun m -> m "3 prefixes inserted.");
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

    Test_log.info (fun m -> m "1 prefix withdrawn");
    OS.Time.sleep_ns (Duration.of_sec (Key_gen.wtime ())) >>= fun () ->

    close_session flow1
    >>= fun () ->
    close_session flow2
    >>= fun () ->

    Test_log.info (fun m -> m "link flapped.");
    OS.Time.sleep_ns (Duration.of_sec (Key_gen.wtime ())) >>= fun () ->

    Lwt.return_unit
  ;;

  let start s =
    (* Enable debug mode *)
    Printexc.record_backtrace true;

    let config = Config_parser.parse_from_file (Key_gen.config ()) in

    Test_log.info (fun m -> m "Tests starts.");
    start_test s config >>= fun () ->
    Test_log.info (fun m -> m "Test finishes.");
    
    Lwt.return_unit
  ;;
end