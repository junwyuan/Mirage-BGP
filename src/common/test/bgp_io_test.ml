(* open Alcotest
open Bgp_io
open Bgp
open Printf


let pfxs_gen seed n =
  let pfxs = ref [] in
  let r = ref seed in
  for i = 1 to n do
    r := Int32.add !r 256_l; 
    let pfx = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_int32 !r) in
    pfxs := pfx::!pfxs;
  done;
  (!pfxs, !r)
;;

let test_util_take () =
  let pfxs = [
    Ipaddr.V4.Prefix.make 8 (Ipaddr.V4.of_string_exn "10.0.0.0");
    Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "66.173.0.0");
    Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "172.168.13.0");
  ] in
  let taken, rest = Util.take pfxs 5 in
  assert (List.length rest = 1);
  assert (List.length taken = 2);
  let taken2, rest2 = Util.take pfxs 6 in
  assert (List.length rest2 = 1);
  assert (List.length taken2 = 2);
  let taken3, rest3 = Util.take pfxs 9 in
  assert (List.length rest3 = 0);
  assert (List.length taken3 = 3);
;;

let test_util_split () =
  let pfxs, _ = pfxs_gen (Int32.shift_left 128_l 24) 1000 in
  let split = Util.split pfxs (4096 - 23) in
  assert (List.length split = 1);

  let pfxs, _ = pfxs_gen (Int32.shift_left 128_l 24) 2000 in
  let split = Util.split pfxs (4096 - 23) in
  assert (List.length split = 2);
;;


let test_split_update () =
  let withdrawn, _ = pfxs_gen (Int32.shift_left 128_l 24) 1000 in
  let update = { withdrawn; path_attrs = []; nlri = [] } in
  let split = Util.split_update update in
  assert (List.length split = 1);

  let withdrawn, _ = pfxs_gen (Int32.shift_left 128_l 24) 2000 in
  let update = { withdrawn; path_attrs = []; nlri = [] } in
  let split = Util.split_update update in
  assert (List.length split = 2);

  let flags = {
    transitive = false;
    optional = false;
    partial = false;
    extlen = false;
  } in
  let path_attrs = [
    (flags, Bgp.Origin Bgp.EGP); 
    (flags, Bgp.As_path [Bgp.Asn_seq [5_l; 2_l]]);
    (flags, Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1")); 
  ] in
  let withdrawn, _ = pfxs_gen (Int32.shift_left 128_l 24) 2000 in
  let nlri, _ = pfxs_gen (Int32.shift_left 128_l 24) 2000 in
  let update = { withdrawn; path_attrs; nlri } in
  assert (List.length (Util.split_update update) = 4);
;;



let () =
  run "bgp" [
    "util", [
      test_case "test util.take" `Slow test_util_take;
      test_case "test util.split" `Slow test_util_split;
      test_case "test Util.split_update" `Slow test_split_update;
    ]
  ]
;;

   *)