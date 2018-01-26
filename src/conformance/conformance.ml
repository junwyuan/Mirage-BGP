open Lwt.Infix
open Bgp
open Relay

let conf_log = Logs.Src.create "Conf" ~doc:"Conformance tests log"
module Conf_log = (val Logs.src_log conf_log : Logs.LOG)

module Ip_gen = struct
  let default_seed = Int32.shift_left 128_l 24
  
  let next seed = Int32.add seed 256_l
end

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

  let dut_asn () =
    match Key_gen.speaker () with
    | "quagga" -> 1_l
    | "dev" -> 10_l
    | _ -> 10_l
  ;;

  let dut_ip () =
    match Key_gen.speaker () with
    | "quagga" -> Ipaddr.V4.of_string_exn "172.19.0.2"
    | "dev" -> Ipaddr.V4.of_string_exn "172.19.0.3"
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
            fail test_name "Create_session: wrong msg type (not OPEN)"
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

  let close_session flow = 
    Bgp_flow.write flow (Notification Cease)
    >>= function
    | Error _ -> fail "unknown" "fail to write"
    | Ok () -> Bgp_flow.close flow
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
        let n_seed = Ip_gen.next seed in
        let nlri = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_int32 n_seed) ] in
        let u = { withdrawn; path_attrs; nlri } in
        Bgp_flow.write flow (Bgp.Update u)
        >>= function
        | Error _ -> fail test_name "fail to write update"
        | Ok () -> plain_feed test_name flow (count + 1) n_seed
    in

    (* via speaker1, load the router with prefixes *)
    plain_feed test_name flow1 0 Ip_gen.default_seed 
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

  let test_route_withdraw s () =
    let test_name = "route withdrawn" in
    
    create_session s (relay1 ()) test_name
    >>= fun (flow1, _) ->
    create_session s (relay2 ()) test_name
    >>= fun (flow2, _) ->

    (* Write 1st update *)
    let nlri = [ Ipaddr.V4.Prefix.make 8 sample_id] in
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
              if Ipaddr.V4.Prefix.network pfx = sample_id then 
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
                      if Ipaddr.V4.Prefix.network pfx = sample_id then
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

  let test_route_replace s () =
    let test_name = "test route replace" in
    
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

                assert (nlri = [ Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "55.19.24.0") ]);
                assert (find_aspath path_attrs = Some [ Asn_seq [dut_asn (); (relay2 ()).as_no; 100_l; 2_l]]);
                assert (find_next_hop path_attrs = Some (dut_ip ()));

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
    let test_name = "test route unchange case" in
    
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
    | Error _ -> assert false
    | Ok () -> 
      (* Check that I receive the first update *)
      let rec rloop1 () =
        match%lwt Bgp_flow.read flow2 with
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
          match%lwt Bgp_flow.write flow2 (Update update) with
          | Error _ -> fail test_name "fail to write update 2"
          | Ok () ->
            (* Check the 2nd update *)
            let rec rloop2 () =
              match%lwt Bgp_flow.read flow1 with
              | Error _ -> fail test_name "fail to read update 2"
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
    let test_name = "test header error handle" in
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
    Conf_log.info (fun m -> m "Tests start.");
    let tests = [
      (* test_create_session s; 
      test_maintain_session s;
      test_propagate_update_to_old_peer s;
      test_propagate_update_to_new_peer s;
      test_propagate_group_update_to_new_peer s;
      test_route_withdraw s;
      test_route_replace s;
      test_route_unchanged s; *)
      test_header_error_handle s;
      test_update_attr_length_error_handle s;
    ] in
    run tests
    >>= fun () ->
    Conf_log.info (fun m -> m "All pass.");
    Lwt.return_unit
  ;;
end