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
    | _ -> Relay.dev_relay1
  ;;

  let relay2 () = 
    match Key_gen.speaker () with
    | "quagga" -> Relay.quagga_relay2
    | _ -> Relay.dev_relay2
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
    let open Bgp in
    let open Relay in
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
    let open Relay in
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
          Bgp_flow.close flow 
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
                Bgp_flow.close flow1
                >>= fun () ->
                Bgp_flow.close flow2
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
                Bgp_flow.close flow1
                >>= fun () ->
                Bgp_flow.close flow2
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
          let open Bgp in
          let flags = { optional=false; transitive=true; extlen=false; partial=false } in
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
        | Error _ -> fail test_name "tcp write fail"
        | Ok () -> plain_feed test_name flow (count + 1) n_seed
    in

    (* via speaker1, load the router with prefixes *)
    plain_feed test_name flow1 0 Ip_gen.default_seed 
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
            | [ pfx ] ->
              if Ipaddr.V4.Prefix.network pfx = sample_id then 
                Bgp_flow.close flow1
                >>= fun () ->
                Bgp_flow.read flow2
                >>= function
                | Error _ -> fail test_name "tcp read fail"
                | Ok msg -> match msg with
                  | Open _ | Keepalive | Notification _ -> fail test_name "wrong msg type"
                  | Update { withdrawn } ->
                    match withdrawn with
                    | [ pfx ] ->
                      if Ipaddr.V4.Prefix.network pfx = sample_id then
                        close_session flow2
                        >>= fun () ->
                        close_session flow1
                        >>= fun () ->
                        pass test_name
                      else fail test_name "incorrect withdrawn prefix"
                    | _ -> fail test_name "incorrect withdrawns"
              else
                fail test_name "update message contain wrong info"
            | _ -> fail test_name "update message contain wrong info"
          end
          | _ -> fail test_name "wrong msg type" 
      in
    read_loop ()
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
    Conf_log.info (fun m -> m "test starts");
    let tests = [
      (* test_create_session s;  *)
      (* test_maintain_session s; *)
      (* test_propagate_update_to_old_peer s;
      test_propagate_update_to_new_peer s;
      test_propagate_group_update_to_new_peer s; *)
      test_route_withdraw s;
    ] in
    run tests
    >>= fun () ->
    Conf_log.info (fun m -> m "All test passes");
    Lwt.return_unit
  ;;
end