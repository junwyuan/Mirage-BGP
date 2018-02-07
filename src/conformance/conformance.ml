open Lwt.Infix
open Bgp
open Relay

let (>>>) a b = a >>= fun () -> b

let conf_log = Logs.Src.create "Conf" ~doc:"Conformance tests log"
module Conf_log = (val Logs.src_log conf_log : Logs.LOG)

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

  let dut_asn () =
    match Key_gen.speaker () with
    | "quagga" -> 1_l
    | "xen" -> 1_l
    | "xorp" -> 1_l
    | "frr" -> 1_l
    | "bird" -> 1_l
    | _ -> 10_l
  ;;

  let dut_ip () =
    match Key_gen.speaker () with
    | "quagga" -> Ipaddr.V4.of_string_exn "172.19.0.2"
    | "frr" -> Ipaddr.V4.of_string_exn "172.19.0.6"
    | "xorp" -> Ipaddr.V4.of_string_exn "172.19.0.7"
    | "bird" -> Ipaddr.V4.of_string_exn "172.19.0.8"
    | _ -> Ipaddr.V4.of_string_exn "172.19.0.3"
  ;;

  let default_hold_time = 180

  let sample_id = Ipaddr.V4.of_string_exn "10.0.0.0"

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
  
  let default_update () = 
    let withdrawn = [] in
    let nlri = [ Ipaddr.V4.Prefix.make 8 sample_id] in
    let path_attrs = [
      Origin EGP;
      Next_hop (relay1 ()).local_id;
      As_path [Bgp.Asn_seq [(relay1 ()).local_asn; 2_l; 3_l]]
    ] in
    Update { withdrawn; nlri; path_attrs}
  ;;

  let is_empty_update { withdrawn; path_attrs; nlri } = withdrawn = [] && nlri = []

  let fail test_name fail_reason = 
    Conf_log.err (fun m -> m "Test %s fails: %s" test_name fail_reason);
    assert false
  ;;

  let pass test_name = 
    Conf_log.info (fun m -> m "Pass test %s" test_name);
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
    | Error _ -> 
      Log.err (fun m -> m "Error when read");
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


  let create_session s relay test_name =
    let open Relay in
    Bgp_flow.create_connection s (relay.remote_id, relay.remote_port)
    >>= function
    | Error _ -> assert false
    | Ok flow ->
      send_msg flow (default_open_msg relay) (module Conf_log)
      >>= fun () ->

      Bgp_flow.read flow
      >>= function
      | Error _ -> assert false
      | Ok msg ->
        match msg with
        | Keepalive | Notification _ | Update _ -> 
          Conf_log.err (fun m -> m "Expect OPEN but rec: %s" (to_string msg));
          assert false
        | Open o ->
          send_msg flow Bgp.Keepalive (module Conf_log)
          >>= fun () ->

          let cond = function
            | Keepalive -> true
            | msg -> 
              Conf_log.err (fun m -> m "Expect Keepalive but rec: %s" (to_string msg));
              assert false
          in
          read_loop flow cond (module Conf_log)
          >>= fun () ->

          Lwt.return (flow, o)
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

  

  let test_create_session s () = 
    let test_name = "create session" in
    create_session s (relay1 ()) test_name
    >>= fun (flow, _) ->
    close_session flow
    >>= fun () ->
    pass test_name
  ;;
  
  let test_maintain_session s () =
    let test_name = "maintain session" in

    create_session s (relay1 ()) test_name
    >>= fun (flow, o) ->
    
    let negotiated_ht = min o.hold_time default_hold_time in
    let cond = function
      | Keepalive -> true
      | Update _ -> false
      | msg -> 
        Sp1_log.err (fun m -> m "Expect Keepalive but rec: %s" (to_string msg));
        assert false
    in
    Lwt.pick [
      read_loop flow cond (module Sp1_log);
      OS.Time.sleep_ns (Duration.of_sec negotiated_ht);
    ]
    >>= fun () ->

    close_session flow
    >>= fun () ->
    pass test_name
  ;;

  let test_propagate_update_to_new_peer s () =
    let test_name = "propagate update to new peer" in
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    
    send_msg flow1 (default_update ()) (module Sp1_log)
    >>= fun () ->

    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    let cond u = 
      match u.nlri with 
      | [ pfx ] ->
        if Ipaddr.V4.Prefix.network pfx = sample_id then true
        else assert false
      | _ -> assert false
    in
    read_update_loop flow2 cond (module Sp2_log)
    >>= fun () ->

    close_session flow1
    >>= fun () ->
    close_session flow2
    >>= fun () ->
    pass test_name
  ;;

  let test_propagate_update_to_old_peer s () =
    let test_name = "propagate update to old peer" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    send_msg flow1 (default_update ()) (module Sp1_log)
    >>= fun () ->

    let cond u = 
      match u.nlri with 
      | [ pfx ] ->
        if Ipaddr.V4.Prefix.network pfx = sample_id then true
        else assert false
      | _ -> assert false
    in
    read_update_loop flow2 cond (module Sp2_log)
    >>= fun () ->

    close_session flow1
    >>= fun () ->
    close_session flow2
    >>= fun () ->
    pass test_name
  ;;

  let test_no_propagate_update_to_src s () =
    let test_name = "don't propagate update to src" in
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    
    send_msg flow1 (default_update ()) (module Sp1_log)
    >>= fun () ->

    Lwt.pick [ 
      read_update_loop flow1 (fun u -> assert false) (module Sp1_log);   
      OS.Time.sleep_ns (Duration.of_sec 5)
    ]
    >>= fun () ->
    close_session flow1
    >>= fun () ->
    pass test_name
  ;;

  module Prefix_set = Set.Make(Ipaddr.V4.Prefix)
  let test_propagate_group_update_to_new_peer s () =
    let test_name = "group propagate update to new peer" in
    let group_size = 500 in
    (* connect to first speaker *)
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->

    let rec plain_feed test_name flow count seed =
      if count >= group_size then
        Lwt.return_unit
      else 
        let withdrawn = [] in
        let path_attrs =
          let origin = Origin EGP in
          let next_hop = Next_hop (relay1 ()).local_id in
          let as_path = As_path [Asn_seq [(relay1 ()).local_asn; 2_l; 3_l]] in
          [origin; next_hop; as_path]
        in
        let nlri, n_seed = Pfx_gen.gen seed 1 in
        let u = { withdrawn; path_attrs; nlri } in
        Bgp_flow.write flow (Bgp.Update u)
        >>= function
        | Error _ -> fail test_name "fail to write update"
        | Ok () -> plain_feed test_name flow (count + 1) n_seed
    in

    (* via speaker1, load the router with prefixes *)
    plain_feed test_name flow1 0 Pfx_gen.default_seed 
    >>= fun () ->

    (* Allow installation *)
    OS.Time.sleep_ns (Duration.of_sec 1)
    >>= fun () ->

    (* connect to speaker2 *)
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->
    let rec read_loop (set: Prefix_set.t) =
      Bgp_flow.read flow2
      >>= function
      | Error _ -> fail test_name "tcp read fail"
      | Ok msg -> 
        match msg with
        | Keepalive -> read_loop set
        | Update { withdrawn; path_attrs; nlri } -> begin
          (* update recorded ids *)
          let new_set = List.fold_left (fun acc pfx -> Prefix_set.add pfx acc) set nlri in
          (* check for completeness *)
          if Prefix_set.cardinal new_set = 500 then 
            close_session flow1
            >>= fun () ->
            close_session flow2
            >>= fun () ->
            pass test_name
          else
            read_loop new_set
        end
        | _ -> fail test_name "wrong msg type" 
    in
    read_loop Prefix_set.empty
  ;;

  let test_simul_insert s () =
    let test_name = "simultaneous insert" in
    let cluster_size = 500 in
    
    (* connect to speakers *)
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    let t1 () =
      let rloop = read_loop_count_nlri flow2 1000 (module Sp2_log : Logs.LOG) in

      let path_attrs =
        let origin = Origin EGP in
        let next_hop = Next_hop (relay1 ()).local_id in
        let as_path = As_path [Asn_seq [(relay1 ()).local_asn; 2_l; 3_l]] in
        [origin; next_hop; as_path]
      in
      plain_feed  ~cluster_size ~total:1000
                  ~seed:(Pfx_gen.default_seed) ~flow:flow1 
                  ~path_attrs (module Sp1_log : Logs.LOG)
      >>= fun _ ->
      rloop
    in

    let t2 () =
      let rloop = read_loop_count_nlri flow1 1000 (module Sp1_log : Logs.LOG) in

      let path_attrs =
        let origin = Origin EGP in
        let next_hop = Next_hop (relay2 ()).local_id in
        let as_path = As_path [Asn_seq [(relay2 ()).local_asn; 2_l; 3_l]] in
        [origin; next_hop; as_path]
      in
      plain_feed  ~cluster_size ~total:1000 
                  ~seed:(Pfx_gen.peek_next_n Pfx_gen.default_seed 3000) 
                  ~flow:flow2 
                  ~path_attrs (module Sp2_log : Logs.LOG)
      >>= fun _ ->
      rloop
    in

    Lwt.join [t1 (); t2 ()]
    >>= fun () ->


    (* Clean up *)
    close_session flow1 
    >>= fun () ->
    close_session flow2
    >>= fun () ->
    pass test_name
  ;;

  let test_link_flap s () =
    let test_name = "link flap" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    (* Write 1st update *)
    let update = 
      let nlri = [ 
        Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
      ] in
      let path_attrs = [
        Origin EGP;
        Next_hop (relay1 ()).local_id;
        As_path [Bgp.Asn_seq [(relay1 ()).local_asn; 2_l; 3_l]]
      ] in
      { withdrawn = []; nlri; path_attrs} 
    in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    (* Verify *)
    read_update_loop flow2 (fun u -> assert (List.length u.nlri = 1); true) (module Sp2_log)
    >>= fun () ->

    (* Flap *)
    close_session flow1
    >>= fun () ->

    (* Verify withdrawn *)
    let cond u =
      match u.withdrawn with
      | [ pfx ] ->
        if Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0") = pfx then true
        else assert false
      | _ -> assert false
    in
    read_update_loop flow2 cond (module Sp2_log)
    >>= fun () ->

    close_session flow2
    >>= fun () ->
    pass test_name
  ;;

  let test_route_withdrawn s () =
    let test_name = "route withdrawn" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    (* Write 1st update *)
    let update = 
      let nlri = [ 
        Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
        Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.2.0");
      ] in
      let path_attrs = [
        Origin EGP;
        Next_hop (relay1 ()).local_id;
        As_path [Bgp.Asn_seq [(relay1 ()).local_asn; 2_l; 3_l]]
      ] in
      { withdrawn = []; nlri; path_attrs} 
    in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->
    
    (* Verify the message *)
    read_update_loop flow2 (fun u -> true) (module Sp2_log)
    >>= fun () ->

    (* 2nd update to withdrawn one of the prefixes *)
    let update = 
      let wd = [
        Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
      ] in
      { withdrawn = wd; path_attrs = []; nlri = [] }
    in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    (* Verify the withdrawn *)
    let cond u = 
      if u.withdrawn = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0") ] then true
      else assert false
    in
    read_update_loop flow2 cond (module Sp2_log)
    >>= fun () ->

    close_session flow1
    >>= fun () ->
    close_session flow2
    >>= fun () ->
    pass test_name
  ;;

  let test_route_selection_after_wd s () =
    let test_name = "route route selection after withdrawn" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name 
    >>= fun (flow2, _) ->

    (* update from speaker 1 *)
    let update = 
      let nlri = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0"); ] in
      let path_attrs = [
        Origin EGP;
        Next_hop (relay1 ()).local_id;
        As_path [Bgp.Asn_seq [(relay1 ()).local_asn; 2_l; 3_l]]
      ] in
      { withdrawn = []; nlri; path_attrs } 
    in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    (* Verify from speaker 2 *)
    read_update_loop flow2 (fun u -> true) (module Sp2_log)
    >>= fun () ->

    (* update from speaker 2 and the updated route is not taken *)
    let update = 
      let nlri = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0"); ] in
      let path_attrs = [
        Origin EGP;
        Next_hop (relay2 ()).local_id;
        As_path [Bgp.Asn_seq [(relay2 ()).local_asn; 65002_l; 65003_l; 65004_l]]
      ] in
      { withdrawn = []; nlri; path_attrs } 
    in
    send_msg flow2 (Update update) (module Sp2_log)
    >>= fun () ->


    (* Withdrawn speaker 1's path *)
    let update = {
      withdrawn = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0"); ];
      path_attrs = [];
      nlri = [];
    } in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    (* Verify from speaker 2 *)
    read_update_loop flow2 (fun u -> assert (List.length u.withdrawn = 1); true) (module Sp2_log)
    >>= fun () ->

    (* Verify that the new best path is advertised *)
    read_update_loop flow1 (fun u -> assert (List.length u.nlri = 1); true) (module Sp1_log)
    >>= fun () ->
    
    (* Send another update and expect no update *)
    let update = 
      let nlri = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0"); ] in
      let path_attrs = [
        Origin EGP;
        Next_hop (relay1 ()).local_id;
        As_path [Bgp.Asn_seq [(relay1 ()).local_asn; 65002_l; 65003_l; 65004_l; 65005_l ]]
      ] in
      { withdrawn = []; nlri; path_attrs} 
    in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    (* verify that no update *)
    Lwt.pick [
      read_update_loop flow2 (fun u -> assert false) (module Sp2_log);
      OS.Time.sleep_ns (Duration.of_sec 5);
    ]
    >>= fun () ->

    close_session flow1
    >>= fun () ->
    close_session flow2
    >>= fun () ->
    pass test_name
  ;;



  let test_route_withdrawn_diff_src s () =
    let test_name = "route withdrawn diff src" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    (* update from speaker 1 *)
    let update = 
      let nlri = [ 
        Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
      ] in
      let path_attrs = [
        Origin EGP;
        Next_hop (relay1 ()).local_id;
        As_path [Bgp.Asn_seq [(relay1 ()).local_asn; 2_l; 3_l]]
      ] in
      { withdrawn = []; nlri; path_attrs} 
    in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    (* Rec from speaker 2 *)
    read_update_loop flow2 (fun u -> true) (module Sp2_log)
    >>= fun () ->
    
    (* update from speaker 2 *)
    let update = 
      let nlri = [ 
        Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.2.0");
      ] in
      let path_attrs = [
        Origin EGP;
        Next_hop (relay2 ()).local_id;
        As_path [Bgp.Asn_seq [(relay2 ()).local_asn; 2_l; 3_l]]
      ] in
      { withdrawn = []; nlri; path_attrs} 
    in
    send_msg flow2 (Update update) (module Sp2_log)
    >>= fun () ->

    (* Rec from speaker 1 *)
    read_update_loop flow1 (fun u -> true) (module Sp1_log)
    >>= fun () ->

    (* Withdrawn 128.0.1.0 from speaker 1 *)
    let update = { 
      withdrawn = [
        Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
      ]; 
      path_attrs = []; nlri = [] 
    } in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    let cond u =
      assert (u.withdrawn = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0")]);
      true
    in
    read_update_loop flow2 cond (module Sp2_log)
    >>= fun () ->

    close_session flow1
    >>= fun () ->
    close_session flow2
    >>= fun () ->
    pass test_name
  ;;

  let test_route_replace s () =
    let test_name = "route replace" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    (* Write the first update *)
    let nlri = [Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.24.0")] in
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ (relay1 ()).local_asn; 100_l; 65002_l; 65003_l ] ];
      Next_hop (relay1 ()).local_id;
    ] in
    let update = { withdrawn = []; nlri; path_attrs } in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    (* Verify 1st update *)
    let cond u =
      assert (List.length u.nlri = 1);
      true
    in
    read_update_loop flow2 cond (module Sp2_log)
    >>= fun () ->

    (* Write the 2nd update *)
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ (relay2 ()).local_asn; 65001_l; 65002_l ] ];
      Next_hop (relay2 ()).local_id;
    ] in
    let update = { withdrawn = []; nlri; path_attrs } in
    send_msg flow2 (Update update) (module Sp2_log)
    >>= fun () ->

    (* Verify the 2nd update *)
    let cond ({ nlri; path_attrs; withdrawn } as u) =
      Sp2_log.debug (fun m -> m "%s" (update_to_string u));
      assert (nlri = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.24.0") ]);
      assert (find_aspath path_attrs = Some [ Asn_seq [dut_asn (); (relay2 ()).local_asn; 65001_l; 65002_l]]);
      true
    in
    read_update_loop flow1 cond (module Sp1_log)
    >>= fun () ->

    close_session flow1
    >>= fun () ->
    close_session flow2
    >>= fun () ->
    pass test_name
  ;;

  let test_route_unchanged s () =
    let test_name = "route unchange case" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    let nlri = [Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.24.0")] in

    (* Write the first update *)
    let update = 
      let path_attrs = [
        Origin EGP;
        As_path [ Asn_seq [ (relay1 ()).local_asn; 100_l; 2_l; 3_l ] ];
        Next_hop (relay1 ()).local_id;
      ] in
      { withdrawn = []; nlri; path_attrs } 
    in
    send_msg flow1 (Update update) (module Sp1_log)
    >>= fun () ->

    (* Check that I receive the first update *)
    read_update_loop flow2 (fun msg -> true) (module Sp2_log)
    >>= fun () ->

    (* Write the 2nd update *)
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ (relay2 ()).local_asn; 100_l; 2_l; 3_l; 20_l ] ];
      Next_hop (relay2 ()).local_id;
    ] in
    let update = {withdrawn = []; nlri; path_attrs } in
    send_msg flow2 (Update update) (module Sp2_log)
    >>= fun () ->

    (* Fail there is an update *)
    Lwt.pick [ 
      read_update_loop flow1 (fun u -> assert false) (module Sp2_log); 
      OS.Time.sleep_ns (Duration.of_sec 5)
    ]
    >>= fun () ->
    
    (* This is the correct case, no update *)
    (* Clean up *)
    close_session flow1
    >>= fun () ->
    close_session flow2
    >>= fun () ->
    pass test_name
  ;;


  (* TODO *)
  let test_msg_error_handle_base s () = Lwt.return_unit

  let test_header_error_handle s () = 
    let test_name = "header error handle" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow, _) ->
    let tcp_flow = Bgp_flow.tcp_flow flow in
    
    let buf = Cstruct.create 19 in
    Cstruct.BE.set_uint16 buf 16 19;
    Cstruct.set_uint8 buf 18 4;
    send_buf tcp_flow buf (module Sp1_log)
    >>= fun () ->

    let cond msg = 
      match msg with
      | Keepalive -> false
      | _ ->
        if (msg = Notification (Message_header_error Connection_not_synchroniszed)) then true
        else begin
          Sp1_log.err (fun m -> m "Faulty msg: %s" (to_string msg));
          assert false
        end
    in
    read_loop flow cond (module Sp1_log)
    >>= fun () -> 

    close_session flow
    >>= fun () ->
    pass test_name
  ;;

  let test_update_attr_length_error_handle s () = 
    let test_name = "attr length error handle" in

    create_session s (relay1 ()) test_name
    >>= fun (flow, _) ->
    let tcp_flow = Bgp_flow.tcp_flow flow in

    let path_attrs = [
      Origin EGP;
      As_path [Asn_seq [1_l]];
      Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
    ] in
    let nlri = [Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "192.168.45.0")] in
    let buf = gen_msg (Update { withdrawn = []; path_attrs; nlri }) in
    Cstruct.set_uint8 buf 25 2;
    send_buf tcp_flow buf (module Sp1_log)
    >>= fun () ->

    let cond msg = 
      match msg with
      | Keepalive -> false
      | Notification (Update_message_error (Attribute_length_error _)) -> true
      | _ ->
        Sp1_log.err (fun m -> m "Faulty msg: %s" (to_string msg));
        assert false
    in
    read_loop flow cond (module Sp1_log)
    >>= fun () -> 

    close_session flow
    >>= fun () ->
    pass test_name
  ;;

  let rec run tests = 
    let interval = 
      match (Key_gen.speaker ()) with
      | "quagga" -> 30
      | _ -> 10
    in
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

    Conf_log.info (fun m -> m "Tests start.");
    let tests = [
      (* test_create_session s; 
      test_maintain_session s;
      test_no_propagate_update_to_src s;
      test_propagate_update_to_old_peer s;
      test_propagate_update_to_new_peer s;
      test_propagate_group_update_to_new_peer s;
      test_simul_insert s;
      test_route_withdrawn s;
      test_route_withdrawn_diff_src s;
      test_link_flap s;
      test_route_replace s;
      test_route_unchanged s;
      test_header_error_handle s;
      test_update_attr_length_error_handle s; *)
      test_route_selection_after_wd s;
    ] in
    run tests
    >>= fun () ->
    Conf_log.info (fun m -> m "All pass.");
    Lwt.return_unit
  ;;
end