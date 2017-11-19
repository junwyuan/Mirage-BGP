open Mirage

let main = foreign "Replay.Main" (console @-> stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "replay" [
    main $ default_console $ stack
  ]