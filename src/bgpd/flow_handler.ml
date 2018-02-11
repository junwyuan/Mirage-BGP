open Bgp
open Lwt.Infix

let flow_src = Logs.Src.create "Flow" ~doc:"Flow Handler logging"
module Flow_log = (val Logs.src_log flow_src : Logs.LOG)

let peer_tag : Ipaddr.V4.t Logs.Tag.def =
  Logs.Tag.def "peer" ~doc:"Peer id" (fun ppf id -> Format.fprintf ppf "%s" (Ipaddr.V4.to_string id))

let stamp id = Logs.Tag.(empty |> add peer_tag id)

module Make (S: Mirage_stack_lwt.V4) = struct
  module Bgp_flow = Bgp_io.Make(S)

  type msg_count = {
    opent: int;
    update: int;
    keepalive: int;
    notif: int;
  }

  let inc_count c msg = 
    match msg with
    | Open _ -> {
      opent = c.opent + 1;
      update = c.update;
      keepalive = c.keepalive;
      notif = c.notif;
    }
    | Update _ -> {
      opent = c.opent;
      update = c.update + 1;
      keepalive = c.keepalive;
      notif = c.notif;
    }
    | Keepalive -> {
      opent = c.opent;
      update = c.update;
      keepalive = c.keepalive + 1;
      notif = c.notif;
    }
    | Notification _ -> {
      opent = c.opent;
      update = c.update;
      keepalive = c.keepalive;
      notif = c.notif + 1;
    }
  ;;

  type stat = {
    sent: msg_count;
    received: msg_count;
  }

  let update_sent_count stat msg = {
    sent = inc_count stat.sent msg;
    received = stat.received;
  }

  let update_rec_count stat msg = {
    sent = stat.sent;
    received = inc_count stat.received msg;
  }
    
  type t = {
    remote_id: Ipaddr.V4.t;
    remote_asn: int32;
    callback: Fsm.event -> unit;
    flow: Bgp_flow.t;
    stream: Bgp.t Lwt_stream.t;
    pf: Bgp.t option -> unit;
    stat: stat;
  }

  let set_stat t n_stat = {
    remote_id = t.remote_id;
    remote_asn = t.remote_asn;
    callback = t.callback;
    flow = t.flow;
    stream = t.stream;
    pf = t.pf;
    stat = n_stat;
  }

  let rec flow_writer t = 
    Lwt_stream.get t.stream >>= function
    | None -> Bgp_flow.close t.flow
    | Some msg -> 
      Flow_log.debug (fun m -> m "send message %s" (Bgp.to_string msg) 
                                            ~tags:(stamp t.remote_id));
      Bgp_flow.write t.flow msg
      >>= function
      | Error err ->
        let () = match err with
          | `Timeout -> Flow_log.debug (fun m -> m "Timeout when write %s" 
                                      (Bgp.to_string msg) ~tags:(stamp t.remote_id))
          | `Refused -> Flow_log.debug (fun m -> m "Refused when Write %s" 
                                      (Bgp.to_string msg) ~tags:(stamp t.remote_id))
          | `Closed -> Flow_log.debug (fun m -> m "Connection closed when write %s." 
                                      (Bgp.to_string msg) ~tags:(stamp t.remote_id)) 
          | _ -> ()
        in
        Lwt.return_unit
      | Ok () -> 
        let new_t = set_stat t (update_sent_count t.stat msg) in
        flow_writer new_t
  ;;

  let rec flow_reader t =
    Bgp_flow.read t.flow >>= function 
    | Ok msg -> 
      let event = match msg with
        | Bgp.Open o -> 
          (* Open message err checking *)
          if o.version <> 4 then Fsm.Bgp_open_msg_err (Unsupported_version_number 4)
          
          (* This is not exactly what the specification indicates *)
          else if o.local_asn <> t.remote_asn then Fsm.Bgp_open_msg_err Bad_peer_as
          
          else Fsm.BGP_open o
        | Bgp.Update u -> begin
          match u.nlri with
          | [] -> 
            (* Do not perform attribute check if no route is advertised *)
            Fsm.Update_msg u
          | _ ->
            match find_aspath u.path_attrs with
            | None -> Fsm.Update_msg_err (Missing_wellknown_attribute 2)
            | Some [] -> Fsm.Update_msg_err Malformed_as_path
            | Some (hd::tl) ->
              match hd with
              | Asn_seq l -> 
                if List.hd l <> t.remote_asn then Fsm.Update_msg_err Malformed_as_path
                else Fsm.Update_msg u
              | Asn_set l ->
                if List.mem t.remote_asn l then Fsm.Update_msg_err Malformed_as_path
                else Fsm.Update_msg u
        end
        | Bgp.Notification e -> Fsm.Notif_msg e
        | Bgp.Keepalive -> Fsm.Keepalive_msg
      in
      Flow_log.debug (fun m -> m "receive message %s" (Bgp.to_string msg) ~tags:(stamp t.remote_id));
      
      (* Spawn thread to handle the new message *)
      t.callback event;

      (* Load balancing *)
      OS.Time.sleep_ns (Duration.of_ms 1)
      >>= fun () ->

      let new_t = set_stat t (update_rec_count t.stat msg) in
      flow_reader new_t
    | Error err ->
      let () = match err with
        | `Closed -> 
          Flow_log.debug (fun m -> m "Connection closed when read." ~tags:(stamp t.remote_id));
          t.callback Fsm.Tcp_connection_fail
        | `Refused -> 
          Flow_log.debug (fun m -> m "Read refused." ~tags:(stamp t.remote_id));
          t.callback Fsm.Tcp_connection_fail
        | `Timeout -> 
          Flow_log.debug (fun m -> m "Read timeout." ~tags:(stamp t.remote_id));
          t.callback Fsm.Tcp_connection_fail
        | `PARSE_ERROR err -> begin
          match err with
          | Bgp.Parsing_error -> 
            Flow_log.warn (fun m -> m "Message parsing error" ~tags:(stamp t.remote_id));
            (* I don't know what the correct event for this should be. *)
            t.callback Fsm.Tcp_connection_fail
          | Bgp.Msg_fmt_error err -> begin
            Flow_log.warn (fun m -> m "Message format error" ~tags:(stamp t.remote_id));
            match err with
            | Bgp.Parse_msg_h_err sub_err -> t.callback (Fsm.Bgp_header_err sub_err)
            | Bgp.Parse_open_msg_err sub_err -> t.callback (Fsm.Bgp_open_msg_err sub_err)
            | Bgp.Parse_update_msg_err sub_err -> t.callback (Fsm.Update_msg_err sub_err)
          end
          | Bgp.Notif_fmt_error _ -> 
            Flow_log.err (fun m -> m "Got an notification message error" ~tags:(stamp t.remote_id));
            (* I don't know what the correct event for this should be. *)
            (* I should log this event locally *)
            t.callback Fsm.Tcp_connection_fail
        end
        | _ -> 
          Flow_log.debug (fun m -> m "Unknown read error in flow reader" ~tags:(stamp t.remote_id));
      in
      Lwt.return_unit
  ;;

  let create remote_id remote_asn callback flow =
    let stream, pf = Lwt_stream.create () in
    let stat = {
      sent = {
        opent = 0;
        update = 0;
        keepalive = 0;
        notif = 0;
      };
      received = {
        opent = 0;
        update = 0;
        keepalive = 0;
        notif = 0;
      };
    } in
    let t = { stream; pf; remote_id; remote_asn; callback; flow; stat } in
    let _ = flow_writer t in
    let _ = flow_reader t in
    t
  ;;

  let stop t = t.pf None

  let write t msg = t.pf (Some msg)
end