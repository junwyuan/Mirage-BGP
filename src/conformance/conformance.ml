open Lwt.Infix
open Bgp
open Relay

let conf_log = Logs.Src.create "Conf" ~doc:"Conformance tests log"
module Conf_log = (val Logs.src_log conf_log : Logs.LOG)

module Ip_gen = struct
  let default_seed = 
    Int32.shift_left 128_l 24
  
  let next seed = 
    let s = Int32.add seed 256_l in
    (Afi.IPv4 s, s)
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

  let default_open_msg () = 
    let open Bgp in
    let open Relay in
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 ((relay1 ()).id);
      my_as = Asn (Int32.to_int ((relay1 ()).as_no));
      options = [];
      hold_time = default_hold_time;
    } in
    Bgp.Open o
  ;;
  

  let default_update () = 
    let open Bgp in
    let open Relay in
    let withdrawn = [] in
    let nlri = [(Afi.IPv4 (Ipaddr.V4.to_int32 sample_id), 8)] in
    let flags = {
      optional=false;
      transitive=true;
      partial=false;
      extlen=false;
    } in
    let path_attrs = [
      (flags, Origin EGP);
      (flags, Next_hop (Ipaddr.V4.to_int32 (relay1 ()).id));
      (flags, As_path [Bgp.Seq [(relay1 ()).as_no; 2_l; 3_l]])
    ] in
    Update {withdrawn; nlri; path_attrs}
  ;;

  let fail test_name fail_reason = 
    Conf_log.info (fun m -> m "Test %s fails: %s" test_name fail_reason);
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

  let create_connection s (addr, port) test_name =
    let open Relay in
    Bgp_flow.create_connection s (addr, port)
    >>= function
    | Error _ -> fail test_name "tcp connect fail"
    | Ok flow ->
      Bgp_flow.write flow (default_open_msg ())
      >>= function
      | Error _ -> fail test_name "tcp write fail"
      | Ok () ->
        Bgp_flow.read flow
        >>= function
        | Error _ -> fail test_name "tcp read fail"
        | Ok msg ->
          let open Bgp in
          match msg with
          | Keepalive | Notification _ | Update _ -> 
            fail test_name "wrong msg type"
          | Open o ->
            Bgp_flow.write flow Bgp.Keepalive
            >>= function
            | Error _ -> fail test_name "tcp write fail"
            | Ok () ->
              Bgp_flow.read flow
              >>= function
              | Error _ -> fail test_name "tcp read fail"
              | Ok msg ->
                match msg with
                | Keepalive -> 
                  Lwt.return (flow, o)
                | _ -> fail test_name "wrong msg type"
  ;;

  let test_create_session s () = 
    let test_name = "create session" in
    let open Relay in
    create_connection s (relay_id (), (relay1 ()).port) test_name
    >>= fun (flow, _) ->
    Bgp_flow.close flow
    >>= fun () ->
    pass test_name
  ;;
  
  let test_maintain_session s () =
    let test_name = "maintain session" in
    let open Relay in
    create_connection s (relay_id (), (relay1 ()).port) test_name
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
    create_connection s (relay_id (), (relay1 ()).port) test_name
    >>= fun (flow1, _) ->
    Bgp_flow.write flow1 (default_update ())
    >>= function
    | Error _ -> fail test_name "tcp write error"
    | Ok () ->
      create_connection s (relay_id (), (relay2 ()).port) test_name
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
            | [(Afi.IPv4 ip, _)] ->
              if ip = Ipaddr.V4.to_int32 sample_id then 
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
    create_connection s (relay_id (), (relay1 ()).port) test_name
    >>= fun (flow1, _) ->
    create_connection s (relay_id (), (relay2 ()).port) test_name
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
            | [(Afi.IPv4 ip, _)] ->
              if ip = Ipaddr.V4.to_int32 sample_id then 
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


  module Int32_set = Set.Make(Int32)

  let test_propagate_group_update_to_new_peer s () =
    let test_name = "group propagate update to new peer" in
    let group_size = 500 in
    (* connect to first speaker *)
    create_connection s (relay_id (), (relay1 ()).port) test_name
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
          let next_hop = Next_hop (Ipaddr.V4.to_int32 sample_id) in
          let as_path = As_path [Seq [4_l; 2_l; 3_l]] in
          List.map (fun pa -> (flags, pa)) [origin; next_hop; as_path]
        in
        let ip, n_seed = Ip_gen.next seed in
        let nlri = [(ip, 24)] in
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
    create_connection s (relay_id (), (relay2 ()).port) test_name
    >>= fun (flow2, _) ->
    let rec read_loop set =
      Bgp_flow.read flow2
      >>= function
      | Error _ -> fail test_name "tcp read fail"
      | Ok msg -> 
        match msg with
        | Keepalive -> read_loop set
        | Update { withdrawn; path_attrs; nlri } -> begin
          (* update recorded ids *)
          let f acc (ip, _) =
            match ip with
            | Afi.IPv4 ip -> Int32_set.add ip acc
            | Afi.IPv6 _ -> acc
          in
          let new_set = List.fold_left f set nlri in
          
          (* check for completeness *)
          let rec check i =
            if (i >= group_size) then
              Bgp_flow.close flow1
              >>= fun () ->
              Bgp_flow.close flow2
              >>= fun () ->
              pass test_name
            else
              let i32 = Int32.of_int i in
              let tmp = Int32.add Ip_gen.default_seed (Int32.mul i32 256_l) in
              match Int32_set.mem tmp new_set with
              | true ->  
                check (i + 1)
              | false -> read_loop new_set
          in
          check 1
        end
        | _ -> fail test_name "wrong msg type" 
    in
    read_loop Int32_set.empty
  ;;

  let rec run tests = 
    match tests with
    | test::other -> 
      test ()
      >>= fun () ->
      OS.Time.sleep_ns (Duration.of_sec 1)
      >>= fun () ->
      run other
    | [] -> Lwt.return_unit
  ;;

  let start s =
    Conf_log.info (fun m -> m "test starts");
    let tests = [
      test_create_session s; 
      test_maintain_session s;
      test_propagate_update_to_old_peer s;
      test_propagate_update_to_new_peer s;
      test_propagate_group_update_to_new_peer s
    ] in
    run tests
    >>= fun () ->
    Conf_log.info (fun m -> m "All test passes");
    Lwt.return_unit
  ;;
end