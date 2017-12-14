(* Assumptions: *)
(* 1. S.TCPV4.read flow gives back a buffer. If the number of data bytes n is smaller than 4096
then the buffer has length n, otherwise, the buffer has size 4096. The rest data is given in
next read. *)

open Lwt.Infix

module type S = sig
  type t
  type flow
  type error
  type write_error
  
  val create_connection : t -> (flow, error) Result.result Lwt.t
end

let io_log = Logs.Src.create "IO" ~doc:"IO LOG"
module Io_log = (val Logs.src_log io_log : Logs.LOG)

module Make (S: Mirage_stack_lwt.V4) = struct
  type flow = S.TCPV4.flow
  
  type error = private [>
  | `Closed
  | `BGP_MSG_ERR of Bgp.error
  | Mirage_protocols.Tcp.error
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

  let rec parse b msgs =
    if (Cstruct.len b < 19) then (Ok msgs, Some b)
    else if (Cstruct.len b < Bgp.get_h_len b) then (Ok msgs, Some b)
    else
      let msg_len = Bgp.get_h_len b in
      let msg_buf, rest = Cstruct.split b msg_len in
      let parsed = Bgp.parse_buffer_to_t msg_buf in
      match parsed with 
      | Result.Ok (msg, _) ->
        let acc = List.cons msg msgs in
        if Cstruct.len rest = 0 then (Ok acc, None)
        else parse rest acc
      | Result.Error err -> (Error err, None)
  ;;

  let burst_read t : (Bgp.t list, error) Result.result Lwt.t =
    S.TCPV4.read t.flow
    >>= function
    | Ok (`Data b) -> begin
      let unparsed = match t.buf with
      | None -> b
      | Some v -> Cstruct.concat [v; b]
      in 

      let result, unparsed = parse unparsed [] in
      match result with
      | Ok [] -> 
        Io_log.err (fun m -> m "This is a marker. This situation should never occur."); 
        Lwt.fail_with "This is a marker. This situation should never occur in BGP_IO.read"
      | Ok msgs ->
        t.buf <- unparsed;
        Lwt.return (Ok msgs)
      | Error err ->
        Lwt.return (Error (`BGP_MSG_ERR err))
    end
    | Ok (`Eof) -> Lwt.return (Error `Closed)
    | Error err -> begin
      match err with 
      | `Refused -> Lwt.return (Error `Refused)
      | `Timeout -> Lwt.return (Error `Timeout)
      | _ -> 
        Io_log.err (fun m -> m "This is a marker. This situation should never occur."); 
        Lwt.fail_with "This is a marker. This situation should never occur in BGP_IO.read"
    end
  ;;

  let read t : (Bgp.t, error) Result.result Lwt.t = 
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
          Io_log.err (fun m -> m "This is a marker. This situation should never occur."); 
          Lwt.fail_with "This is a marker. This situation should never occur in BGP_IO.read"
      end
      | Ok (`Data b) -> begin
        if (Cstruct.len b < 19) || (Cstruct.len b < Bgp.get_h_len b) then begin
          Io_log.err (fun m -> m "This is a marker. This situation should never occur."); 
          Lwt.fail_with "This is a marker. This situation should never occur in BGP_IO.read"
        end
        else begin
          let msg_len = Bgp.get_h_len b in
          let msg_buf, rest = Cstruct.split b msg_len in
          if Cstruct.len rest > 0 then t.buf <- Some rest;
          let parsed = Bgp.parse_buffer_to_t msg_buf in
          match parsed with 
          | Result.Ok (msg, _) -> Lwt.return (Ok msg)
          | Result.Error err -> Lwt.return (Error (`BGP_MSG_ERR err))
        end
      end)
    | Some buf ->
      
  ;;


  let write t msg = 


    

  


end