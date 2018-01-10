open Alcotest
open Bgp_io

let ip4_of_ints a b c d =
  Int32.of_int ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
;;

let test_util_take () =
  let pfxs = [
    (Afi.IPv4 (ip4_of_ints 10 0 0 0), 8);
    (Afi.IPv4 (ip4_of_ints 66 173 0 0), 16);
    (Afi.IPv4 (ip4_of_ints 172 168 13 0), 24);
  ] in
  let rest, taken = Util.take pfxs 5 [] in
  assert (List.length rest = 1);
  assert (List.length taken = 2);
  let rest2, taken2 = Util.take pfxs 6 [] in
  assert (List.length rest2 = 1);
  assert (List.length taken2 = 2);
  let rest3, taken3 = Util.take pfxs 9 [] in
  assert (List.length rest3 = 0);
  assert (List.length taken3 = 3);
;;


let () =
  run "bgp" [
    "util", [
      test_case "test util take" `Slow test_util_take;
    ]
  ]
;;

  