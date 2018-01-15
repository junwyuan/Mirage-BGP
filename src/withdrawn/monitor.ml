open Lwt.Infix
open Relay
open Bgp

let mon_log = Logs.Src.create "Monitor" ~doc:"Monitor log"
module Mon_log = (val Logs.src_log mon_log : Logs.LOG)

module Ip_gen = struct
  let (+++) x y = Int32.add x y

  let (>>>) x y = Int32.shift_right x y
  let (<<<) x y = Int32.shift_left x y
  let (&&&) x y = Int32.logand x y
  let (|||) x y = Int32.logor x y

  let default_seed = 
    Int32.shift_left 128_l 24

  let peek_next_n seed n =
    Int32.add seed (Int32.mul 256_l (Int32.of_int n))
  ;;

  let next seed = 
    let s = 
      if (seed >>> 8) &&& 0xff_l = 255_l then seed +++ 512_l else seed +++ 256_l
    in
    (Afi.IPv4 s, s)
  ;;

  let next_n seed n =
    let rec loop seed n acc =
      match n = 0 with
      | true -> (acc, seed)
      | false ->
        let ip, new_seed = next seed in
        loop new_seed (n - 1) (ip::acc)
    in
    loop seed n []
  ;;
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
  
  let default_open_msg relay = 
    let o = {
      version = 4;
      bgp_id = Ipaddr.V4.to_int32 relay.id;
      my_as = Asn (Int32.to_int relay.as_no);
      options = [];
      hold_time = default_hold_time;
    } in
    Bgp.Open o
  ;;

  let fail msg = 
    Mon_log.err (fun m -> m "%s" msg);
    Lwt.fail_with msg
  ;;

  let create_connection s relay =
    let open Relay in
    Bgp_flow.create_connection s (relay_id (), relay.port)
    >>= function
    | Error _ -> fail "tcp connect fail"
    | Ok flow ->
      Bgp_flow.write flow (default_open_msg relay)
      >>= function
      | Error _ -> fail "tcp write fail"
      | Ok () ->
        Bgp_flow.read flow
        >>= function
        | Error _ -> fail "tcp read fail"
        | Ok msg ->
          let open Bgp in
          match msg with
          | Keepalive | Notification _ | Update _ -> 
            fail "wrong msg type"
          | Open o ->
            Bgp_flow.write flow Bgp.Keepalive
            >>= function
            | Error _ -> fail "tcp write fail"
            | Ok () ->
              Bgp_flow.read flow
              >>= function
              | Error _ -> fail "tcp read fail"
              | Ok msg ->
                match msg with
                | Keepalive ->
                  Lwt.return (flow, o)
                | _ -> fail "wrong msg type"
  ;;

  let close flow = 
    Bgp_flow.write flow (Bgp.Notification Bgp.Cease)
    >>= function
    | Error _ -> fail "tcp write fail"
    | Ok () ->
      Bgp_flow.close flow
  ;;

  module Replay = struct
    let replay_log = Logs.Src.create "Replay" ~doc:"Logging for Replay"
    module Replay_log = (val Logs.src_log replay_log : Logs.LOG)

    let rec plain_feed flow count total cluster_size seed =
      if count >= total then Lwt.return_unit
      else 
        let withdrawn = [] in
        let path_attrs = 
          let open Bgp in
          let flags = { optional=false; transitive=true; extlen=false; partial=false } in
          let origin = Origin EGP in
          let next_hop = Next_hop (Ipaddr.V4.to_int32 (relay1 ()).id) in
          let as_path = As_path [Seq [4_l; 2_l; 3_l]] in
          List.map (fun pa -> (flags, pa)) [origin; next_hop; as_path]
        in
        let ips, n_seed = Ip_gen.next_n seed cluster_size in
        let nlri = List.map (fun ip -> (ip, 24)) ips in
        let msg =  Bgp.Update { withdrawn; path_attrs; nlri } in
        Replay_log.debug (fun m -> m "%s" (Bgp.to_string msg));
        Bgp_flow.write flow msg
        >>= function
        | Error _ ->
          Replay_log.err (fun m -> m "tcp write fail");
          Lwt.fail_with "tcp write fail"
        | Ok () -> plain_feed flow (count + cluster_size) total cluster_size n_seed
    ;;

    let rec read_loop flow =
      Bgp_flow.read flow
      >>= function 
      | Ok msg -> 
        Replay_log.debug (fun m -> m "Receive %s" (Bgp.to_string msg));
        read_loop flow
      | Error err ->
        (match err with 
        | `Refused -> Replay_log.debug (fun m -> m "Read refused")
        | `Timeout -> Replay_log.debug (fun m -> m "Read timeout")
        | `Closed -> Replay_log.debug (fun m -> m "Connection closed when read.")
        | `BGP_MSG_ERR err ->
          (match err with
          | Bgp.Parsing_error -> Replay_log.debug (fun m -> m "Parsing error")
          | _ -> Replay_log.debug (fun m -> m "Msg format error"))
        | _ -> ()); 
        Lwt.return_unit
    ;;
  end

  module Receiver = struct
    let rec_log = Logs.Src.create "receiver" ~doc:"Receiver log"
    module Rec_log = (val Logs.src_log rec_log : Logs.LOG)

    let count = ref 0

    let is_completed msg total = 
      let is_marker ip = 
        match ip with
        | Afi.IPv6 _ -> false
        | Afi.IPv4 v -> Int32.compare v total == 0
      in
      match msg with
      | Update { withdrawn; nlri; path_attrs } ->
        List.exists (fun (ip, _) -> is_marker ip) nlri
      | _ -> false
    ;;

    let rec read_loop flow pfxs_count total =
      Bgp_flow.read flow
      >>= function 
      | Ok msg -> 
        count := !count + 1;
        Rec_log.debug (fun m -> m "Receive pkg %d: %s" (!count) (Bgp.to_string msg));
        (match msg with
        | Update { withdrawn; nlri; path_attrs } ->
          let new_count = pfxs_count + List.length withdrawn in
          if new_count = total then Lwt.return_unit 
          else read_loop flow new_count total
        | _ -> read_loop flow pfxs_count total)
      | Error err ->
        (match err with 
        | `Refused -> Rec_log.debug (fun m -> m "Read refused")
        | `Timeout -> Rec_log.debug (fun m -> m "Read timeout")
        | `Closed -> Rec_log.debug (fun m -> m "Connection closed when read.")
        | `BGP_MSG_ERR err ->
          (match err with
          | Bgp.Parsing_error -> Rec_log.debug (fun m -> m "Parsing error")
          | _ -> Rec_log.debug (fun m -> m "Msg format error"))
        | _ -> ()); 
        Lwt.return_unit
    ;;

    let rec write_keepalive_loop flow =
      OS.Time.sleep_ns (Duration.of_sec 30) 
      >>= fun () ->
      Bgp_flow.write flow Bgp.Keepalive
      >>= function
      | Error _ ->
        Rec_log.warn (fun m -> m "write keepalive failed. This may be expected.");
        write_keepalive_loop flow
      | Ok () ->
        write_keepalive_loop flow
    ;;
  end
  
  let start_throughput_test s total seed =
    let min_ad_intvl = 
      match (Key_gen.speaker ()) with
      | "quagga" -> 5
      | _ -> 5
    in
    (* connect to speaker1 *)
    create_connection s (relay1 ())
    >>= fun (flow1, _) ->
    let replay_rloop = Replay.read_loop flow1 in
    (* connect to speaker2 *)
    create_connection s (relay2 ())
    >>= fun (flow2, _) ->
    (* Keepalive loop *)
    let wk_loop = Receiver.write_keepalive_loop flow2 in
    (* Listen speaker2 *)
    let marker_id = Ip_gen.peek_next_n seed total in
    let rec_rloop = Receiver.read_loop flow2 0 total in
    (* Wait out minimal advertisement time *)
    OS.Time.sleep_ns (Duration.of_sec min_ad_intvl)
    >>= fun () ->
    (* start feeding speaker1 *)
    Replay.plain_feed flow1 0 total 500 seed
    >>= fun () ->
    OS.Time.sleep_ns (Duration.of_sec 5)
    >>= fun () ->
    let start_time = Unix.gettimeofday () in
    close flow1
    >>= fun () ->
    let sending_time = Unix.gettimeofday () -. start_time in
    Lwt.pick [wk_loop; rec_rloop]
    >>= fun () ->
    let processing_time = Unix.gettimeofday () -. start_time in
    Mon_log.info (fun m -> m "Test size: %d, sending time: %fs, Processing time: %fs"
                  total sending_time processing_time);
    (* Close flows *)
    close flow2
    >>= fun () ->
    Lwt.return marker_id
  ;;

  let start s =
    let test_sizes = [1000; 10000; 50000; 100000] in
    let rec loop seed = function
      | [] -> Lwt.return_unit
      | hd::tl -> 
        start_throughput_test s hd seed
        >>= fun new_seed ->
        (* Allow router to recover *)
        let interval = 
          match (Key_gen.speaker ()) with
          | "quagga" -> 30
          | _ -> 5
        in
        OS.Time.sleep_ns (Duration.of_sec interval)
        >>= fun () ->
        loop new_seed tl
    in
    loop (Ip_gen.default_seed) test_sizes
  ;;
end