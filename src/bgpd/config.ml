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
  let doc = Key.Arg.info ~doc:"Flag indicating whether interact with OS" ["kernel"] in
  Key.(create "kernel" Arg.(flag doc))
;;

let quota = 
  let doc = Key.Arg.info ~doc:"Quota for controlling how long one peer's input can occupy the CPU" ["quota"] in
  Key.(create "quota" Arg.(opt int 10 doc))
;;

let remove = 
  let doc = Key.Arg.info ~doc:"Flag indicating whether remove the learned routes from kernel's routing table when exit" ["remove"] in
  Key.(create "remove" Arg.(flag doc))
;;

let peer_group_transit = 
  let doc = Key.Arg.info ~doc:"Flag indicating whether transit within peer group is allowed." ["pg_transit"] in
  Key.(create "pg_transit" Arg.(flag doc))
;;

let disk = generic_kv_ro "config"

let gc = 
  let doc = Key.Arg.info ~doc:"whether to give gc alarm." ["gc"] in
  Key.(create "gc" Arg.(flag doc))
;;

let main = foreign 
  ~keys:[ 
    Key.abstract config;
    Key.abstract test;
    Key.abstract runtime;
    Key.abstract kernel;
    Key.abstract remove;
    Key.abstract peer_group_transit;
    Key.abstract gc;
    Key.abstract quota;
  ] 
  "Bgpd.Main" 
  (console @-> kv_ro @-> stackv4 @-> job)



let stack = generic_stackv4 default_network

let () =
  register "bgpd" [
    main $ default_console $ disk $ stack
  ]
;;