open Mirage

let config = 
  let doc = Key.Arg.info ~doc:"This is the path to config file." ["config"] in
  Key.(create "config" Arg.(opt string "config.json" doc))
;;

let start_time = 
  let doc = 
    Key.Arg.info ~doc:"This is the time to wait after connection establishment before start the test."
    ["start_time"]
  in
  Key.(create "start_time" Arg.(opt int 3 doc))
;;

let test = 
  let doc = 
    Key.Arg.info ~doc:"Choose what perf to perform."
    ["test"]
  in
  Key.(create "test" Arg.(opt string "insert phased throughput" doc))

let pfx_per_msg =
  let doc = 
    Key.Arg.info ~doc:"Number of prefixes per message." ["p"]
  in
  Key.(create "pfx_per_msg" Arg.(opt int 100 doc))
;;

let msg_per_round = 
  let doc = 
    Key.Arg.info ~doc:"Number of messages sent per rond." ["m"]
  in
  Key.(create "msg_per_round" Arg.(opt int 500 doc))
;;

let repeat =
  let doc = 
    Key.Arg.info ~doc:"Number of time the test should be repeated" ["r"]
  in
  Key.(create "repeat_times" Arg.(opt int 1 doc))
;;

let verbose =
  let doc = 
    Key.Arg.info ~doc:"Decompile." ["d"]
  in
  Key.(create "decompile" Arg.(flag doc))
;;


let main = 
  let keys = [ 
    Key.abstract config;
    Key.abstract start_time;
    Key.abstract test;
    Key.abstract pfx_per_msg;
    Key.abstract msg_per_round;
    Key.abstract verbose;
    Key.abstract repeat;
  ] in
  foreign ~keys "Perf.Main" (stackv4 @-> job)
;;

let stack = generic_stackv4 default_network

let () =
  register "perf" [
    main $ stack
  ]