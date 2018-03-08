open Mirage

let speaker = 
  let doc = Key.Arg.info ~doc:"Connect to DUT or Quagga." ["speaker"] in
  Key.(create "speaker" Arg.(opt string "dev" doc))
;;

let main = 
  let keys = [ 
    Key.abstract speaker
  ] in
  foreign ~keys "Conformance.Main" (stackv4 @-> job)
;;

let stack = generic_stackv4 default_network

let () =
  register "conformance" [
    main $ stack
  ]