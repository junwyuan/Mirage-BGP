open Mirage

let main = foreign "Receive.Main" (console @-> stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "receive.byte" [
    main $ default_console $ stack
  ]