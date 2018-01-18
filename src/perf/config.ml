open Mirage

let remote_id = 
  let doc = Key.Arg.info ~doc:"This is the remote bgp speaker's ip address." ["remote_id"] in
  Key.(create "remote_id" Arg.(opt string "172.19.0.2" doc))
;;

let remote_port = 
  let doc = Key.Arg.info ~doc:"This is the remote bgp speaker's port." ["remote_port"] in
  Key.(create "remote_port" Arg.(opt int 179 doc))
;;

let local_id = 
  let doc = Key.Arg.info ~doc:"This is the bgp identifier, i.e. ip address." ["local_id"] in
  Key.(create "local_id" Arg.(opt string "172.19.0.3" doc))
;;

let local_port = 
  let doc = Key.Arg.info ~doc:"This is the local bgp speaker's port." ["local_port"] in
  Key.(create "local_port" Arg.(opt int 179 doc))
;;

let local_asn = 
  let doc = Key.Arg.info ~doc:"This is the asn number for this bgp speaker." ["local_asn"] in
  Key.(create "local_asn" Arg.(opt int 2 doc))
;;

let filename = 
  let doc = Key.Arg.info ~doc:"This is the replay file." ["filename"] in
  Key.(create "filename" Arg.(opt string "" doc))
;;

let burst = 
  let doc = Key.Arg.info ~doc:"This is the replay mode." ["burst"] in
  Key.(create "burst" Arg.(opt bool true doc))
;;

let speaker = 
  let doc = Key.Arg.info ~doc:"Connect to DUT or Quagga." ["speaker"] in
  Key.(create "speaker" Arg.(opt string "dev" doc))
;;


let total =
  let doc = Key.Arg.info ~doc:"Total number of packages replayed" ["total"] in
  Key.(create "total" Arg.(opt int 10000 doc))
;;

let main = 
  let keys = [ 
    (* Key.abstract local_asn;  *)
    (* Key.abstract local_port; *)
    (* Key.abstract local_id; *)
    (* Key.abstract filename; *)
    Key.abstract burst;
    Key.abstract speaker;
    (* Key.abstract total; *)
  ] in
  foreign ~keys "Perf.Main" (stackv4 @-> job)
;;

let stack = generic_stackv4 default_network

let () =
  register "perf" [
    main $ stack
  ]