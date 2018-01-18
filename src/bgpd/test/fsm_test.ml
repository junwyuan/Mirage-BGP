open Fsm
open Alcotest
open Bgp
open Printf

let test_set_state () =
  let fsm = Fsm.create 30 90 30 in
  let new_fsm = fsm |> set_state CONNECT in
  assert (new_fsm.state == CONNECT)
;;


let test_idle_to_estab () =
  let fsm = create 0 0 0 in
  assert (fsm.state = IDLE);
  let (fsm1, _) = handle fsm (Fsm.Manual_start) in
  let (fsm2, _) = handle fsm1 (Fsm.Tcp_connection_confirmed) in
  let o = { version = 4; my_as = Asn 1; hold_time = 180; options = []; bgp_id = 1_l } in
  let (fsm3, _) = handle fsm2 (Fsm.BGP_open o) in
  let (fsm4, actions) = handle fsm3 (Fsm.Keepalive_msg) in 
  assert (fsm4.state = ESTABLISHED)
;;

let () =
  run "fsm" [
    "Aux function", [
      test_case "test set state" `Slow test_set_state
    ];
    "state transition", [
      test_case "test normal IDLE to ESTABLISHED transition." `Slow test_idle_to_estab
    ];
  ]

