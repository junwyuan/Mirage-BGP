open Lwt.Infix
open Bgp
open Relay

let conf_log = Logs.Src.create "Conf" ~doc:"Conformance tests log"
module Conf_log = (val Logs.src_log conf_log : Logs.LOG)

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
    | "xorp" -> Relay.xorp_relay1
    | _ -> Relay.dev_relay1
  ;;
  

  let relay2 () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2
    | "host" -> Relay.host_relay2
    | "frr" -> Relay.frr_relay2
    | "xorp" -> Relay.xorp_relay2
    | _ -> Relay.dev_relay2
  ;;

  let dut_asn () =
    match Key_gen.speaker () with
    | "quagga" -> 1_l
    | "xorp" -> 1_l
    | "frr" -> 1_l
    | "dev" -> 10_l
    | _ -> 10_l
  ;;

  let dut_ip () =
    match Key_gen.speaker () with
    | "quagga" -> Ipaddr.V4.of_string_exn "172.19.0.2"
    | "dev" -> Ipaddr.V4.of_string_exn "172.19.0.3"
    | "frr" -> Ipaddr.V4.of_string_exn "172.19.0.6"
    | "xorp" -> Ipaddr.V4.of_string_exn "172.19.0.7"
    | _ -> Ipaddr.V4.of_string_exn "172.19.0.3"
  ;;

  let default_hold_time = 180

  let sample_id = Ipaddr.V4.of_string_exn "10.0.0.0"

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
  
  let default_update () = 
    let withdrawn = [] in
    let nlri = [ Ipaddr.V4.Prefix.make 8 sample_id] in
    let path_attrs = [
      Origin EGP;
      Next_hop (relay1 ()).id;
      As_path [Bgp.Asn_seq [(relay1 ()).as_no; 2_l; 3_l]]
    ] in
    Update { withdrawn; nlri; path_attrs}
  ;;

  let fail test_name fail_reason = 
    Conf_log.err (fun m -> m "Test %s fails: %s" test_name fail_reason);
    Lwt.fail_with test_name
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

  let create_session s relay test_name =
    let open Relay in
    Bgp_flow.create_connection s (relay_id (), relay.port)
    >>= function
    | Error _ -> fail test_name "Create_session: tcp connect fail"
    | Ok flow ->
      Bgp_flow.write flow (default_open_msg relay)
      >>= function
      | Error _ -> fail test_name "Create_session: write open fail"
      | Ok () ->
        Bgp_flow.read flow
        >>= function
        | Error _ -> fail test_name "Create_session: read open fail"
        | Ok msg ->
          let open Bgp in
          match msg with
          | Keepalive | Notification _ | Update _ -> 
            Conf_log.info (fun m -> m "Wrong type of message (NOT OPEN): %s" (to_string msg));
            assert false
          | Open o ->
            Bgp_flow.write flow Bgp.Keepalive
            >>= function
            | Error _ -> fail test_name "Create_session: tcp write keepalive fail"
            | Ok () ->
              Bgp_flow.read flow
              >>= function
              | Error _ -> fail test_name "Create_session: tcp read keepalive fail"
              | Ok msg ->
                match msg with
                | Keepalive ->
                  Lwt.return (flow, o)
                | _ -> fail test_name (Printf.sprintf "wrong msg type: %s" (Bgp.to_string msg))
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
        | `Closed_by_local -> Log.err (fun m -> m "Flow closed by local")
        | _ -> ()); 
        Lwt.return_unit
    in
    loop 0
  ;;

  let close_session flow = 
    Bgp_flow.write flow (Notification Cease)
    >>= function
    | Error _ -> fail "unknown" "fail to write"
    | Ok () -> 
      (* Bgp_flow.read flow
      >>= fun _ -> *)
      Bgp_flow.close flow
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
    let open Relay in
    create_session s (relay1 ()) test_name
    >>= fun (flow, o) ->
    let open Bgp in 
    let negotiated_ht = min o.hold_time default_hold_time in
    let rec read_loop () =
      Bgp_flow.read flow
      >>= function
      | Error _ -> fail test_name "tcp read fail"
      | Ok msg -> begin
        match msg with
        | Keepalive -> 
          close_session flow 
          >>= fun () ->
          pass test_name
        | _ -> read_loop ()
      end
    in
    timeout test_name negotiated_ht read_loop    
  ;;

  let test_propagate_update_to_new_peer s () =
    let test_name = "propagate update to new peer" in
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    Bgp_flow.write flow1 (default_update ())
    >>= function
    | Error _ -> fail test_name "tcp write error"
    | Ok () ->
      create_session s (relay2 ()) test_name
      >>= fun (flow2, _) ->
      let rec read_loop () =
        Bgp_flow.read flow2
        >>= function
        | Error _ -> fail test_name "tcp read fail"
        | Ok msg -> 
          match msg with
          | Keepalive -> read_loop ()
          | Update { withdrawn; path_attrs; nlri } -> begin
            match nlri with 
            | [ pfx ] ->
              if Ipaddr.V4.Prefix.network pfx = sample_id then 
                close_session flow1
                >>= fun () ->
                close_session flow2
                >>= fun () ->
                pass test_name
              else
                fail test_name "update message contain wrong info"
            | _ -> fail test_name "update message contain wrong info"
          end
          | _ -> fail test_name "wrong msg type" 
      in
      read_loop ()
  ;;

  let test_propagate_update_to_old_peer s () =
    let test_name = "propagate update to old peer" in
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    OS.Time.sleep_ns (Duration.of_ms 2500)
    >>= fun () ->

    let start_time = Unix.gettimeofday () in
    Bgp_flow.write flow1 (default_update ())
    >>= function
    | Error _ -> fail test_name "tcp write error"
    | Ok () ->
      let rec read_loop () =
        Bgp_flow.read flow2
        >>= function
        | Error _ -> fail test_name "tcp read fail"
        | Ok msg -> 
          match msg with
          | Keepalive -> read_loop ()
          | Update { withdrawn; path_attrs; nlri } -> begin
            match nlri with 
            | [pfx] ->
              
              Conf_log.info (fun m -> m "Time taken: %.4fs" (Unix.gettimeofday () -. start_time));

              if Ipaddr.V4.Prefix.network pfx = sample_id then 
                close_session flow1
                >>= fun () ->
                close_session flow2
                >>= fun () ->
                pass test_name
              else
                fail test_name "update message contain wrong info"
            | _ -> fail test_name "update message contain wrong info"
          end
          | _ -> fail test_name "wrong msg type" 
      in
    read_loop ()
  ;;

  let test_no_propagate_update_to_src s () =
    let test_name = "don't propagate update to src" in
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    
    Bgp_flow.write flow1 (default_update ())
    >>= function
    | Error _ -> assert false
    | Ok () ->
      let rec read_loop () =
        Bgp_flow.read flow1
        >>= function
        | Error _ -> assert false
        | Ok msg -> 
          match msg with
          | Keepalive -> read_loop ()
          | _ -> 
            
            (* OS.Time.sleep_ns (Duration.of_sec 10)
            >>= fun () -> *)
            
            Conf_log.info (fun m -> m "%s" (to_string msg));
            assert false
      in
      Lwt.pick [read_loop (); OS.Time.sleep_ns (Duration.of_sec 10)]
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
          let next_hop = Next_hop (relay1 ()).id in
          let as_path = As_path [Asn_seq [(relay1 ()).as_no; 2_l; 3_l]] in
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
        let next_hop = Next_hop (relay1 ()).id in
        let as_path = As_path [Asn_seq [(relay1 ()).as_no; 2_l; 3_l]] in
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
        let next_hop = Next_hop (relay2 ()).id in
        let as_path = As_path [Asn_seq [(relay2 ()).as_no; 2_l; 3_l]] in
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
    let nlri = [ 
      Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
    ] in
    let path_attrs = [
      Origin EGP;
      Next_hop (relay1 ()).id;
      As_path [Bgp.Asn_seq [(relay1 ()).as_no; 2_l; 3_l]]
    ] in
    let update = { withdrawn = []; nlri; path_attrs} in
    Bgp_flow.write flow1 (Update update)
    >>= function
    | Error _ -> fail test_name "fail to write 1st update"
    | Ok () ->
      let rec read_loop () =
        (* Check the update *)
        Bgp_flow.read flow2
        >>= function
        | Error _ -> fail test_name "read fail within read_loop"
        | Ok msg -> 
          match msg with
          | Keepalive -> read_loop ()
          | Update { withdrawn; path_attrs; nlri } -> begin
            match nlri with 
            | [ pfx ] ->
              if Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0") = pfx then 
                close_session flow1
                >>= fun () ->
                Bgp_flow.read flow2
                >>= function
                | Error _ -> fail test_name "fail to read withdrawn"
                | Ok msg -> match msg with
                  | Open _ | Keepalive | Notification _ -> fail test_name "wrong msg type, expecting update"
                  | Update { withdrawn } ->
                    match withdrawn with
                    | [ pfx ] ->
                      if Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0") = pfx then
                        close_session flow2
                        >>= fun () ->
                        pass test_name
                      else fail test_name "incorrect withdrawn prefix"
                    | _ -> fail test_name "incorrect withdrawns"
              else
                fail test_name "update message contain wrong info"
            | _ -> fail test_name "update message contain wrong info"
          end
          | _ -> fail test_name "read a message of wrong type, expecting the 1st update." 
      in
    read_loop ()
  ;;

  let test_route_withdrawn s () =
    let test_name = "route withdrawn" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    (* Write 1st update *)
    let nlri = [ 
      Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
      Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.2.0");
    ] in
    let path_attrs = [
      Origin EGP;
      Next_hop (relay1 ()).id;
      As_path [Bgp.Asn_seq [(relay1 ()).as_no; 2_l; 3_l]]
    ] in
    let update = { withdrawn = []; nlri; path_attrs} in
    Bgp_flow.write flow1 (Update update)
    >>= function
    | Error _ -> fail test_name "fail to write 1st update"
    | Ok () ->
      let rec read_loop () =
        (* Check the update *)
        Bgp_flow.read flow2
        >>= function
        | Error _ -> fail test_name "read fail within read_loop"
        | Ok msg -> 
          match msg with
          | Keepalive -> read_loop ()
          | Update { withdrawn; path_attrs; nlri } -> begin
            let wd = [
              Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
            ] in
            let update = { withdrawn = wd; path_attrs = []; nlri = [] } in
            Bgp_flow.write flow1 (Update update)
            >>= function
            | Error _ -> assert false
            | Ok () ->
              let rec read_loop () =
                Bgp_flow.read flow2
                >>= function
                | Error _ -> assert false
                | Ok msg -> match msg with
                  | Open _ | Notification _ -> assert false
                  | Keepalive -> read_loop ()
                  | Update { withdrawn } ->
                    match withdrawn with
                    | [ pfx ] ->
                      if Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0") = pfx then
                        close_session flow1
                        >>= fun () ->
                        close_session flow2
                        >>= fun () ->
                        pass test_name
                      else assert false
                    | _ ->
                      Conf_log.info (fun m -> m "%s" (to_string msg));
                      assert false
              in
              read_loop ()
          end
          | _ -> fail test_name "read a message of wrong type, expecting the 1st update." 
      in
    read_loop ()
  ;;

  let test_route_withdrawn_diff_src s () =
    let test_name = "route withdrawn diff src" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    (* Write 1st update *)
    let nlri = [ 
      Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
    ] in
    let path_attrs = [
      Origin EGP;
      Next_hop (relay1 ()).id;
      As_path [Bgp.Asn_seq [(relay1 ()).as_no; 2_l; 3_l]]
    ] in
    let update = { withdrawn = []; nlri; path_attrs} in
    Bgp_flow.write flow1 (Update update)
    >>= function
    | Error _ -> assert false
    | Ok () ->
      let nlri = [ 
        Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.2.0");
      ] in
      let path_attrs = [
        Origin EGP;
        Next_hop (relay2 ()).id;
        As_path [Bgp.Asn_seq [(relay2 ()).as_no; 2_l; 3_l]]
      ] in
      let update = { withdrawn = []; nlri; path_attrs} in
      Bgp_flow.write flow2 (Update update)
      >>= function
      | Error _ -> assert false
      | Ok () ->
        let rec read_loop () =
          (* Check the update *)
          Bgp_flow.read flow2
          >>= function
          | Error _ -> fail test_name "read fail within read_loop"
          | Ok msg -> 
            match msg with
            | Keepalive -> read_loop ()
            | Update { withdrawn; path_attrs; nlri } -> begin
              Sp2_log.info (fun m -> m "%s" (to_string msg));
              
              let wd = [
                Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0");
              ] in
              let update = { withdrawn = wd; path_attrs = []; nlri = [] } in
              Bgp_flow.write flow1 (Update update)
              >>= function
              | Error _ -> assert false
              | Ok () ->
                let rec read_loop () =
                  Bgp_flow.read flow2
                  >>= function
                  | Error _ -> assert false
                  | Ok msg -> 
                    Sp2_log.info (fun m -> m "%s" (to_string msg));
                    match msg with
                    | Open _ | Notification _ -> assert false
                    | Keepalive -> read_loop ()
                    | Update { withdrawn } ->
                      

                      

                      match withdrawn with
                      | [ pfx ] ->
                        if Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "128.0.1.0") = pfx then
                          close_session flow1
                          >>= fun () ->
                          close_session flow2
                          >>= fun () ->
                          pass test_name
                        else assert false
                      | _ ->
                        Conf_log.info (fun m -> m "%s" (to_string msg));
                        assert false
                in
                read_loop ()
            end
            | _ -> fail test_name "read a message of wrong type, expecting the 1st update." 
        in
      read_loop ()
  ;;

  let test_route_replace s () =
    let test_name = "route replace" in
    
    let%lwt (flow1, _) =  create_session s (relay1 ()) test_name in
    let%lwt (flow2, _) = create_session s (relay2 ()) test_name in

    (* Write the first update *)
    let nlri = [Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.24.0")] in
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ (relay1 ()).as_no; 100_l; 2_l; 3_l ] ];
      Next_hop (relay1 ()).id;
    ] in
    let update = { withdrawn = []; nlri; path_attrs } in
    match%lwt Bgp_flow.write flow1 (Update update) with
    | Error _ -> fail test_name "fail to write update 1"
    | Ok () -> 
      (* Check that I receive the first update *)
      let rec rloop1 () =
        match%lwt Bgp_flow.read flow2 with
        | Error _ -> fail test_name "fail to read update 1"
        | Ok Open _ | Ok Notification _ -> fail test_name "read wrong msg type when expecting an update. "
        | Ok Keepalive -> rloop1 ()
        | Ok Update { nlri } ->
          (* More detailed check on this part is carried out in test_propagate_update *)
          assert (List.length nlri = 1);

          (* Write the 2nd update *)
          let path_attrs = [
            Origin EGP;
            As_path [ Asn_seq [ (relay2 ()).as_no; 100_l; 2_l ] ];
            Next_hop (relay2 ()).id;
          ] in
          let update = {withdrawn = []; nlri; path_attrs } in
          match%lwt Bgp_flow.write flow2 (Update update) with
          | Error _ -> fail test_name "fail to write update 2"
          | Ok () ->
            (* Check the 2nd update *)
            let rec rloop2 () =
              match%lwt Bgp_flow.read flow1 with
              | Error _ -> fail test_name "fail to read update 2"
              | Ok Open _ | Ok Notification _ -> fail test_name "read wrong msg type when expecting an update. "
              | Ok Keepalive -> rloop2 ()
              | Ok (Update { nlri; path_attrs } as msg) ->

                Conf_log.info (fun m -> m "%s" (to_string msg));

                assert (nlri = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.24.0") ]);
                assert (find_aspath path_attrs = Some [ Asn_seq [dut_asn (); (relay2 ()).as_no; 100_l; 2_l]]);
                (* assert (find_next_hop path_attrs = Some (dut_ip ())); *)

                (* Clean up *)
                let%lwt () = close_session flow1 in
                let%lwt () = close_session flow2 in 
                pass test_name
            in
            rloop2 ()
      in
      rloop1 ()
  ;;

  let test_route_unchanged s () =
    let test_name = "route unchange case" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    (* Write the first update *)
    let nlri = [Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.24.0")] in
    let path_attrs = [
      Origin EGP;
      As_path [ Asn_seq [ (relay1 ()).as_no; 100_l; 2_l; 3_l ] ];
      Next_hop (relay1 ()).id;
    ] in
    let update = { withdrawn = []; nlri; path_attrs } in
    match%lwt Bgp_flow.write flow1 (Update update) with
    | Error _ -> assert false
    | Ok () -> 
      (* Check that I receive the first update *)
      let rec rloop1 () =
        Bgp_flow.read flow2
        >>= function 
        | Error _ -> fail test_name "fail to read update 1"
        | Ok Open _ | Ok Notification _ -> fail test_name "read wrong msg type when expecting an update. "
        | Ok Keepalive -> rloop1 ()
        | Ok Update { nlri } -> begin
          (* More detailed check on this part is carried out in test_propagate_update *)
          assert (List.length nlri = 1);

          (* Write the 2nd update *)
          let path_attrs = [
            Origin EGP;
            As_path [ Asn_seq [ (relay2 ()).as_no; 100_l; 2_l; 3_l; 20_l ] ];
            Next_hop (relay2 ()).id;
          ] in
          let update = {withdrawn = []; nlri; path_attrs } in
          Bgp_flow.write flow2 (Update update)
          >>= function
          | Error _ -> fail test_name "fail to write update 2"
          | Ok () ->
            (* Check the 2nd update *)
            let rec rloop2 () =
              match%lwt Bgp_flow.read flow1 with
              | Error _ -> Lwt.return_unit
              | Ok Open _ | Ok Notification _ -> fail test_name "read wrong msg type when expecting an update. "
              | Ok Keepalive -> rloop2 ()
              | Ok Update { nlri; path_attrs } ->
                fail test_name "receive an update when none should come."
            in
            
            Lwt.pick [rloop2 (); OS.Time.sleep_ns (Duration.of_sec 5)]
            >>= fun () ->
            (* This is the correct case, no update *)

            (* Clean up *)
            let%lwt () = close_session flow1 in
            let%lwt () = close_session flow2 in 
            pass test_name
      end
      in
      rloop1 ()
  ;;




  let test_header_error_handle s () = 
    let test_name = "test header error handle" in
    let%lwt (flow, _) = create_session s (relay1 ()) test_name in
    let tcp_flow = Bgp_flow.tcp_flow flow in
    
    let buf = Cstruct.create 19 in
    Cstruct.BE.set_uint16 buf 16 19;
    Cstruct.set_uint8 buf 18 4;

    match%lwt S.TCPV4.write tcp_flow buf with
    | Error _ -> assert false
    | Ok () ->
      let rec rloop () =
        match%lwt Bgp_flow.read flow with
        | Error _ -> assert false
        | Ok Keepalive -> rloop ()
        | Ok msg -> 
          assert (msg = Notification (Message_header_error Connection_not_synchroniszed)); 

          pass test_name
      in
      rloop ()
  ;;

  let test_update_attr_length_error_handle s () = 
    let test_name = "attr length error handle" in
    let%lwt (flow, _) = create_session s (relay1 ()) test_name in
    let tcp_flow = Bgp_flow.tcp_flow flow in
    
    let path_attrs = [
      Origin EGP;
      As_path [Asn_seq [1_l]];
      Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
    ] in
    let nlri = [Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "192.168.45.0")] in
    
    let buf = gen_msg (Update { withdrawn = []; path_attrs; nlri }) in

    Cstruct.set_uint8 buf 25 2;

    match%lwt S.TCPV4.write tcp_flow buf with
    | Error _ -> assert false
    | Ok () ->
      let rec rloop () =
        match%lwt Bgp_flow.read flow with
        | Error _ -> assert false
        | Ok Keepalive -> rloop ()
        | Ok msg -> 
          match msg with
          | Notification (Update_message_error (Attribute_length_error _)) ->
            pass test_name
          | _ -> assert false
      in
      rloop ()
  ;;

  
    
  let rec run tests = 
    let interval = 
      match (Key_gen.speaker ()) with
      | "quagga" -> 30
      | _ -> 30
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
    Conf_log.info (fun m -> m "Tests start.");
    let tests = [
      (* test_create_session s; 
      test_maintain_session s;
      test_propagate_update_to_old_peer s;
      test_propagate_update_to_new_peer s;
      test_propagate_group_update_to_new_peer s;
      test_route_withdrawn s;
      test_simul_insert s; *)
      (* test_no_propagate_update_to_src s; *)
      (* test_route_withdrawn_diff_src s; *)
      test_link_flap s;
      test_route_replace s;
      test_route_unchanged s;
      test_header_error_handle s;
      test_update_attr_length_error_handle s;
    ] in
    run tests
    >>= fun () ->
    Conf_log.info (fun m -> m "All pass.");
    Lwt.return_unit
  ;;
end