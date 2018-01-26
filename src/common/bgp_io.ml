(* Assumptions: *)
(* 1. S.TCPV4.read flow gives back a buffer. If the number of data bytes n is smaller than 4096
then the buffer has length n, otherwise, the buffer has size 4096. The rest data is given in
next read. *)

open Lwt.Infix
open Bgp

module Util = struct
  (* let take pfxs len =
    let rec loop pfxs len acc =
      match pfxs with
      | [] -> (acc, [])
      | pfx::tl ->
        let mask = Ipaddr.V4.Prefix.bits pfx in
        let bs = pfxlen_to_bytes mask + 1 in
        if bs > len then 
          (acc, pfxs)
        else 
          loop tl (len - bs) (pfx::acc)
    in
    loop pfxs len []
  ;;

  let rec split pfxs len =
    let rec loop pfxs len acc = 
      let taken, rest = take pfxs len in
      match rest with
      | [] -> taken::acc
      | _ -> loop rest len (taken::acc)
    in
    loop pfxs len []
  ;;

  let split_update update =
    let rec loop ({ withdrawn; path_attrs; nlri } as u) acc = 
      if len_update_buffer u <= 4096 then u::acc
      else
        let len_fixed = 23 in
        let len_wd = len_pfxs_buffer withdrawn in

        if len_wd > 4096 - len_fixed then
          let taken, withdrawn = take withdrawn (4096 - len_fixed) in
          let update = { withdrawn = taken; path_attrs = []; nlri = [] } in
          loop { withdrawn; path_attrs; nlri } (update::acc)
        else
          let len_pa = len_path_attrs_buffer path_attrs in
          if len_wd + len_pa + len_fixed >= 4096 then
            let update = { withdrawn; path_attrs = []; nlri = [] } in
            loop { withdrawn; path_attrs; nlri } (update::acc)
          else if len_wd > 0 then
            let taken, nlri = take nlri (4096 - len_wd - len_pa - len_fixed) in
            let update = { withdrawn; path_attrs; nlri = taken } in
            loop { withdrawn = []; path_attrs; nlri } (update::acc)
          else
            let taken, nlri = take nlri (4096 - len_pa - len_fixed) in
            let update = { withdrawn = []; path_attrs; nlri = taken } in
            loop { withdrawn; path_attrs; nlri } (update::acc)
    in
    loop update []
  ;; *)
end

module type S = sig
  type s
  type t
  type conn_error
  type flow
  type read_error = private [>
  | `Closed
  | `PARSE_ERROR of Bgp.parse_error
  | Mirage_protocols.Tcp.error
  | `Closed_by_local
  ]
  type write_error
  
  val create_connection : s -> Ipaddr.V4.t * int -> (t, conn_error) Result.result Lwt.t
  val listen : s -> int -> (t -> unit Lwt.t) -> unit
  val read : t -> (Bgp.t, read_error) Result.result Lwt.t
  val write : t -> Bgp.t -> (unit, write_error) Result.result Lwt.t
  val close : t -> unit Lwt.t
  val dst : t -> Ipaddr.V4.t * int
  val tcp_flow : t -> flow
end

let io_log = Logs.Src.create "IO" ~doc:"IO LOG"
module Io_log = (val Logs.src_log io_log : Logs.LOG)

module Make (S: Mirage_stack_lwt.V4) : S with type s = S.t 
                                      and type conn_error = S.TCPV4.error 
                                      and type write_error = S.TCPV4.write_error 
                                      and type flow = S.TCPV4.flow = struct
  type s = S.t
  type conn_error = S.TCPV4.error
  type flow = S.TCPV4.flow
  type read_error = private [>
  | `Closed
  | `PARSE_ERROR of Bgp.parse_error
  | Mirage_protocols.Tcp.error
  | `Closed_by_local
  ]
  type write_error = S.TCPV4.write_error

  type t = {
    s: S.t;
    flow: flow;
    mutable buf: Cstruct.t option;
  }

  let create_connection s (addr, port) =
    S.TCPV4.create_connection (S.tcpv4 s) (addr, port)
    >>= function
    | Ok flow -> Lwt.return (Ok { s; flow; buf = None })
    | Error err -> Lwt.return (Error err)
  ;;

  let listen s port callback = 
    let wrapped flow = 
      let t = { s; flow; buf = None } in
      callback t
    in
    S.listen_tcpv4 s port wrapped
  ;;

  let rec read t : (Bgp.t, read_error) Result.result Lwt.t = 
    let parse t b =
      let msg_len = Bgp.get_msg_len b in
      let msg_buf, rest = Cstruct.split b msg_len in
      if Cstruct.len rest > 0 then t.buf <- Some rest else t.buf <- None;
      let parsed = Bgp.parse_buffer_to_t msg_buf in
      match parsed with 
      | Result.Ok msg -> Lwt.return (Ok msg)
      | Result.Error err -> Lwt.return (Error (`PARSE_ERROR err))
    in

    match t.buf with
    | None ->
      S.TCPV4.read t.flow
      >>= (function
      | Ok (`Eof) -> Lwt.return (Error `Closed)
      | Error err -> begin
        match err with 
        | `Refused -> Lwt.return (Error `Refused)
        | `Timeout -> Lwt.return (Error `Timeout)
        | _ -> 
          Lwt.return (Error `Closed_by_local)

      end
      | Ok (`Data b) -> begin
        if (Cstruct.len b < 19) || (Cstruct.len b < Bgp.get_msg_len b) then begin
          t.buf <- Some b;
          read t
        end else parse t b
      end)
    | Some b ->
      if (Cstruct.len b < 19) || (Cstruct.len b < Bgp.get_msg_len b) then
        S.TCPV4.read t.flow
        >>= fun result ->
        match result with
        | Ok (`Data buf) ->
          t.buf <- Some (Cstruct.concat [b; buf]);
          read t
        | Ok (`Eof) -> Lwt.return (Error `Closed)
        | Error err -> begin
          match err with 
          | `Refused -> Lwt.return (Error `Refused)
          | `Timeout -> Lwt.return (Error `Timeout)
          | err ->
            (* Io_log.err (fun m -> m "Unknown TCP read error. Have you closed the flow before reading it?"); 
            Lwt.fail_with "Unknown TCP read error. Have you closed the flow before reading it?" *)
            Lwt.return (Error `Closed_by_local)
        end
      else parse t b
  ;;

  let write t msg = 
    (* match msg with
    | Open _ | Keepalive | Notification _ -> S.TCPV4.write t.flow (Bgp.gen_msg msg)
    | Update u ->
      Io_log.debug (fun m -> m "checkpoint 3");
      let split_updates = Util.split_update u in
      Io_log.debug (fun m -> m "checkpoint 4");
      let rec sending_loop = function
        | [] -> Lwt.return (Ok ())
        | msg::tl -> 
          S.TCPV4.write t.flow (Bgp.gen_msg (Bgp.Update msg))
          >>= fun result ->
          match result with
          | Error _ -> Lwt.return result
          | Ok () -> sending_loop tl
      in
      sending_loop split_updates *)
    S.TCPV4.write t.flow (Bgp.gen_msg msg)
  ;;
  

  let close t = S.TCPV4.close t.flow

  let dst t = S.TCPV4.dst t.flow

  let tcp_flow t = t.flow
end