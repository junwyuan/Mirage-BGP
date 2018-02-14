open Lwt.Infix
open Bgp


let log = Logs.Src.create "Col" ~doc:"Collision tests log"
module Log = (val Logs.src_log log : Logs.LOG)

module Main (S: Mirage_stack_lwt.V4) = struct 
  module Bgp_flow = Bgp_io.Make(S)
  let local_id = Ipaddr.V4.of_string_exn "172.19.0.5"
  let local_asn = 2_l

  let remote_id =  Ipaddr.V4.of_string_exn "172.19.0.3"
  let remote_asn = 10_l
  let local_port = 179
  let remote_port = 179
  let default_open_msg = 
    let o = {
      version = 4;
      local_id;
      local_asn;
      options = [];
      hold_time = 180;
    } in
    Bgp.Open o
  ;;

  let holder, waker = Lwt.wait ()
  

  let is_empty_update { withdrawn; path_attrs; nlri } = withdrawn = [] && nlri = []

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

  let test_open_confirm_collision s flow =
    Log.app (fun m -> m "Receive connection");

    let f = function
      | Open _ -> true
      | _ -> assert false
    in
    read_loop flow f (module Log)
    >>= fun () ->

    send_msg flow default_open_msg (module Log)
    >>= fun () ->

    let f = 
      Bgp_flow.create_connection s (remote_id, remote_port)
      >>= function
      | Error _ -> assert false
      | Ok flow -> Lwt.return @@ flow
    in
    f >>= fun flow2 ->

    send_msg flow2 default_open_msg (module Log)
    >>= fun () ->

    let f = function
      | Notification Cease -> 
        Log.app (fun m -> m "Cease received.");
        true
      | Keepalive -> false
      | msg -> 
        Log.app (fun m -> m "Expect Notif Cease, but got: %s" (to_string msg));
        assert false
    in
    read_loop flow f (module Log)
    >>= fun () ->

    Log.info (fun m -> m "Test pass.");
    Lwt.wakeup waker ();
    Lwt.return_unit
  ;;

  let start s =
    Bgp_flow.listen s local_port (test_open_confirm_collision s);

    holder >>= fun () ->
    Lwt.return @@ ()
  ;;

end