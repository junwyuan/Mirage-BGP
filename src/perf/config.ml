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
  Key.(create "start_time" Arg.(opt int 5 doc))
;;


let main = 
  let keys = [ 
    Key.abstract config;
    Key.abstract start_time;
  ] in
  foreign ~keys "Perf.Main" (stackv4 @-> job)
;;

let stack = generic_stackv4 default_network

let () =
  register "perf" [
    main $ stack
  ]