open Mirage

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
    (* Key.abstract filename; *)
    Key.abstract burst;
    Key.abstract speaker;
    (* Key.abstract total; *)
  ] in
  foreign ~keys "Fairness.Main" (stackv4 @-> job)
;;

let stack = generic_stackv4 default_network

let () =
  register "fairness" [
    main $ stack
  ]