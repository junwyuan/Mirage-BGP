open Mirage
let host = 
  let doc = Key.Arg.info ~doc:"This is the remote bgp speaker's ip address." ["host"] in
  Key.(create "host" Arg.(opt string "172.19.0.2" doc))
;;

let id = 
  let doc = Key.Arg.info ~doc:"This is the bgp identifier, i.e. ip address." ["id"] in
  Key.(create "id" Arg.(opt string "172.19.0.3" doc))
;;

let asn = 
  let doc = Key.Arg.info ~doc:"This is the asn number for this bgp speaker." ["asn"] in
  Key.(create "asn" Arg.(opt int 64513 doc))
;;

let main = foreign ~keys:[Key.abstract host; Key.abstract id; Key.abstract asn] "Receive.Main" (stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "receive" [
    main $ stack
  ]