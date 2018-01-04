open Lwt.Infix

let mon_log = Logs.Src.create "Monitor" ~doc:"Monitor log"
module Mon_log = (val Logs.src_log mon_log : Logs.LOG)

module Main (S: Mirage_stack_lwt.V4) = struct
  module Replayer = Replay.Main(S)
  module Receiver = Receive.Main(S)
  
  let start s = 
    let t1 = Receiver.start s in
    OS.Time.sleep_ns (Duration.of_sec 30)
    >>= fun () ->
    let start_time = Unix.gettimeofday () in
    Replayer.start s
    >>= fun () ->
    Mon_log.info (fun m -> m "Sending ends: %fs" (Unix.gettimeofday () -. start_time));
    t1
    >>= fun () ->
    let end_time = Unix.gettimeofday () in
    Mon_log.info (fun m -> m "Processing time: %fs" (end_time -. start_time));
    Lwt.return_unit
  ;;
end