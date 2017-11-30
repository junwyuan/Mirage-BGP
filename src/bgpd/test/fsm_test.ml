open Fsm
open Alcotest
open Bgp
open Printf

let idle_to_established =
  let f () =
    let fsm = create 0 0 0 in
    assert (fsm.state = IDLE);
    let (fsm1, _) = handle fsm (Fsm.Manual_start) in
    let (fsm2, _) = handle fsm1 (Fsm.Tcp_connection_confirmed) in
    let o = { version = 4; my_as = Asn 1; hold_time = 180; options = []; bgp_id = 1_l } in
    let (fsm3, _) = handle fsm2 (Fsm.BGP_open o) in
    let (fsm4, actions) = handle fsm3 (Fsm.Keepalive_msg) in 
    assert (fsm4.state = ESTABLISHED);

    printf "%d\n" (List.length actions);
    assert (List.length actions = 1);    
  in
  test_case "IDLE to ESTABLISHED state transit test." `Slow f
;;

let () =
  run "fsm" [
    "state transition", [idle_to_established]
  ]

