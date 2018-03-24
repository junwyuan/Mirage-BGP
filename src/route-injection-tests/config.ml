open Mirage

let config = 
  let doc = Key.Arg.info ~doc:"This is the path to config file." ["config"] in
  Key.(create "config" Arg.(opt string "config.json" doc))
;;

let wtime =
  let doc = Key.Arg.info ~doc:"This is the time interval between two operations." ["wtime"] in
  Key.(create "wtime" Arg.(opt int 10 doc))
;;

let main = 
  let keys = [ 
    Key.abstract config;
    Key.abstract wtime;
  ] in
  foreign ~keys "Test.Main" (stackv4 @-> job)
;;

let stack = generic_stackv4 default_network

let () =
  register "test" [
    main $ stack
  ]