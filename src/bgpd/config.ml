open Mirage


let config = 
  let doc = Key.Arg.info ~doc:"This is the path to config within data directory." ["config"] in
  Key.(create "config" Arg.(opt string "config/bgpd.json" doc))
;;

let test = 
  let doc = Key.Arg.info ~doc:"Flag indicating whether running as test mode." ["test"] in
  Key.(create "test" Arg.(flag doc))
;;

let runtime = 
  let doc = Key.Arg.info ~doc:"Used together with test mode, indicate how long before auto shutdown." ["runtime"] in
  Key.(create "runtime" Arg.(opt int 60 doc))
;;

let kernel = 
  let doc = Key.Arg.info ~doc:"Flag indicating whether install the learned routes into kernel's routing table" ["kernel"] in
  Key.(create "kernel" Arg.(flag doc))
;;

let remove = 
  let doc = Key.Arg.info ~doc:"Flag indicating whether remove the learned routes from kernel's routing table when exit" ["remove"] in
  Key.(create "remove" Arg.(flag doc))
;;

let main = foreign 
  ~keys:[ 
    Key.abstract config;
    Key.abstract test;
    Key.abstract runtime;
    Key.abstract kernel;
    Key.abstract remove;
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