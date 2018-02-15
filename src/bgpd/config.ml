open Mirage


let config = 
  let doc = Key.Arg.info ~doc:"This is the path to config within data directory." ["config"] in
  Key.(create "config" Arg.(opt string "bgpd.json" doc))
;;

let main = foreign ~keys:[ Key.abstract config ] "Bgpd.Main" (console @-> kv_ro @-> stackv4 @-> job)

let disk = generic_kv_ro "data"

let stack = generic_stackv4 default_network

let () =
  register "bgpd" [
    main $ default_console $ disk $ stack
  ]
;;