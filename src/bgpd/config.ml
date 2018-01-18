open Mirage

let config = 
  let doc = Key.Arg.info ~doc:"This is the path to config file." ["config"] in
  Key.(create "config" Arg.(opt string "bgpd.json" doc))
;;

let main = foreign ~keys:[ Key.abstract config ] "Bgpd.Main" (stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "bgpd" [
    main $ stack
  ]
;;