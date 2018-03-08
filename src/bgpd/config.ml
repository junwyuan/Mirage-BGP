open Mirage


let config = 
  let doc = Key.Arg.info ~doc:"This is the path to config within data directory." ["config"] in
  Key.(create "config" Arg.(opt string "config/bgpd.json" doc))
;;

let test = 
  let doc = Key.Arg.info ~doc:"Whether it is running as test mode." ["test"] in
  Key.(create "test" Arg.(opt bool false doc))
;;

let runtime = 
  let doc = Key.Arg.info ~doc:"Used together with test mode, indicate how long before auto shutdown." ["runtime"] in
  Key.(create "runtime" Arg.(opt int 60 doc))
;;


let main = foreign 
  ~keys:[ 
    Key.abstract config;
    Key.abstract test;
    Key.abstract runtime;
  ] 
  "Bgpd.Main" 
  (console @-> stackv4 @-> job)

(* let disk = generic_kv_ro "data" *)

let stack = generic_stackv4 default_network

let () =
  register "bgpd" [
    main $ default_console $ stack
  ]
;;