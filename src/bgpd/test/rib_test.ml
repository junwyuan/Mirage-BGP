open Alcotest
open Bgp
open Rib

let test_get_aspath_len () =
  let as_path = [
    Bgp.Asn_set [1_l; 2_l; 3_l];
    Bgp.Asn_seq [4_l; 5_l];
  ] in
  assert (Loc_rib.get_aspath_len as_path = 3)
;;

let test_is_aspath_loop () = 
  let as_path = [
    Bgp.Asn_set [1_l; 2_l; 3_l];
    Bgp.Asn_seq [4_l; 5_l];
  ] in
  assert (Loc_rib.is_aspath_loop 1_l as_path = true);
  assert (Loc_rib.is_aspath_loop 4_l as_path = true);
  assert (Loc_rib.is_aspath_loop 6_l as_path = false);
;;



let test_find_origin () =
  let open Bgp in
  let path_attrs = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l]]);
  ] in
  assert (Loc_rib.find_origin path_attrs = Bgp.EGP)
;;

let test_find_aspath () =
  let open Bgp in
  let path_attrs = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l]]);
  ] in
  assert (Loc_rib.find_aspath path_attrs = [Bgp.Asn_seq [1_l]]);
;;

let test_append_aspath () = 
  let segments = [
    Asn_seq [2_l; 3_l];
    Asn_set [1_l];
  ] in
  let n_segments = Loc_rib.append_aspath 5_l segments in
  assert (List.length n_segments = 2);

  (match n_segments with
  | (Asn_seq l)::tl -> assert (List.length l = 3)
  | _ -> assert false);


  let segments = [
    Asn_set [1_l];
    Asn_seq [2_l; 3_l];
  ] in
  let n_segments = Loc_rib.append_aspath 5_l segments in
  assert (List.length n_segments = 3);

  (match n_segments with
  | (Asn_seq l)::tl -> assert (List.length l = 1)
  | _ -> assert false);
;;



let test_tie_break () =

  (* Depends on as_path length *)
  let id1 = Ipaddr.V4.of_string_exn "172.19.10.2" in
  let id2 = Ipaddr.V4.of_string_exn "172.19.10.1" in
  let pa1 = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l]]);
    (Bgp.Next_hop id1);
  ] in
  let pa2 = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l; 3_l]]);
    (Bgp.Next_hop id2);
  ] in
  assert (Loc_rib.tie_break (pa1, id1) (pa2, id2));

  (* Depends on origin *)
  let id3 = Ipaddr.V4.of_string_exn "172.19.10.2" in
  let id4 = Ipaddr.V4.of_string_exn "172.19.10.1" in
  let pa3 = [
    (Bgp.Origin Bgp.IGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l]]);
    (Bgp.Next_hop id3);
  ] in
  let pa4 = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l]]);
    (Bgp.Next_hop id4);
  ] in
  assert (Loc_rib.tie_break (pa3, id3) (pa4, id4) = false);

  (* Depends on peer id *)
  let id5 = Ipaddr.V4.of_string_exn "172.19.10.2" in
  let id6 = Ipaddr.V4.of_string_exn "172.19.10.1" in
  let pa5 = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l]]);
    (Bgp.Next_hop id5);
  ] in
  let pa6 = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l]]);
    (Bgp.Next_hop id6);
  ] in
  assert (Loc_rib.tie_break (pa5, id5) (pa6, id6) = false);
;;

let test_adj_rib_update_db () = 
  
  (* Test insertion *)
  let path_attrs = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l]]);
    (Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1"));
  ] in
  let nlri = [ (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.19.0.0")); ] in
  let update1 : Rib.update = { withdrawn = []; path_attrs; nlri } in
  
  let db0 = Prefix_map.empty in

  let db1, out_update1 = Adj_rib_in.update_db update1 db0 in
  assert (Prefix_map.cardinal db1 = 1);
  assert (List.length out_update1.nlri = 1);
  assert (List.length out_update1.withdrawn = 0);

  let path_attrs2 = [
    (Bgp.Origin Bgp.IGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l]]);
    (Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1"));
  ] in
  let nlri2 = [
    (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.20.0.0"));
  ] in

  let update2 : Rib.update = { withdrawn = []; path_attrs = path_attrs2; nlri = nlri2 } in
  let db2, out_update2 = Adj_rib_in.update_db update2 db1 in
  assert (Prefix_map.cardinal db2 = 2);
  assert (List.length out_update2.nlri = 1);
  assert (List.length out_update2.withdrawn = 0);

  (* Test replace *)
  let path_attrs3 = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l; 3_l]]);
    (Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1"));
  ] in
  let nlri3 = [ 
    (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.19.0.0")); 
  ] in
  let update3 = { withdrawn = []; path_attrs = path_attrs3; nlri = nlri3 } in

  let db3, out_update3 = Adj_rib_in.update_db update3 db2 in
  assert (Prefix_map.cardinal db3 = 2);
  assert (List.length out_update3.nlri = 1);
  assert (List.length out_update3.withdrawn = 0);

  (* Test withdrawn *)
  let update4 = {
    withdrawn = [
      (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.19.0.0"));
      (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.20.0.0"));
    ];
    path_attrs = [];
    nlri = [];
  } in
  let db4, out_update4 = Adj_rib_in.update_db update4 db3 in
  assert (Prefix_map.cardinal db4 = 0);
  assert (List.length out_update4.nlri = 0);
  assert (List.length out_update4.withdrawn = 2);
;;

let test_loc_rib_update_db () = 
  let local_asn = 1_l in
  let local_id = Ipaddr.V4.of_string_exn "172.19.0.3" in

  
  (* Test insertion *)
  let id1 = Ipaddr.V4.of_string_exn "172.19.10.1" in

  let path_attrs = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [5_l; 2_l]]);
    (Bgp.Next_hop id1);
  ] in
  let nlri = [ (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.19.0.0")); ] in
  let update = { withdrawn = []; path_attrs; nlri } in
  
  let db = Prefix_map.empty in

  let db, out_update = Loc_rib.update_db local_id local_asn (update, id1) db in
  assert (Prefix_map.cardinal db = 1);
  assert (List.length out_update.nlri = 1);
  assert (List.length out_update.withdrawn = 0);

  let id2 = Ipaddr.V4.of_string_exn "172.19.10.2" in
  let path_attrs = [
    (Bgp.Origin Bgp.IGP);
    (Bgp.As_path [Bgp.Asn_seq [5_l; 2_l]]);
    (Bgp.Next_hop id2);
  ] in
  let nlri = [ (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.20.0.0")); ] in
  let update : Rib.update = { withdrawn = []; path_attrs = path_attrs; nlri = nlri } in
  
  let db, out_update = Loc_rib.update_db local_id local_asn (update, id2) db in
  assert (Prefix_map.cardinal db = 2);
  assert (List.length out_update.nlri = 1);
  assert (List.length out_update.withdrawn = 0);

  (* Test loop detection *)

  let id2 = Ipaddr.V4.of_string_exn "172.19.10.2" in
  let path_attrs = [
    (Bgp.Origin Bgp.IGP);
    (Bgp.As_path [Bgp.Asn_seq [1_l; 2_l]]);
    (Bgp.Next_hop id2);
  ] in
  let nlri = [ (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.20.0.0")); ] in
  let update : Rib.update = { withdrawn = []; path_attrs = path_attrs; nlri = nlri } in
  
  let db, out_update = Loc_rib.update_db local_id local_asn (update, id2) db in
  assert (Prefix_map.cardinal db = 2);
  assert (List.length out_update.nlri = 0);
  assert (List.length out_update.withdrawn = 0);


  (* Test replace *)
  let path_attrs = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [5_l; 2_l; 3_l]]);
    (Bgp.Next_hop id1);
  ] in
  let nlri = [ 
    (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.19.0.0")); 
  ] in
  let update = { withdrawn = []; path_attrs; nlri } in

  let db, out_update = Loc_rib.update_db local_id local_asn (update, id1) db in
  assert (Prefix_map.cardinal db = 2);
  assert (List.length out_update.nlri = 1);
  assert (List.length out_update.withdrawn = 0);

  (* This advertised route would not be chosen as its as_path is longer than the current one *)
  let path_attrs = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [5_l; 2_l; 3_l; 4_l]]);
    (Bgp.Next_hop id2);
  ] in
  let nlri = [ 
    (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.19.0.0")); 
  ] in
  let update = { withdrawn = []; path_attrs = path_attrs; nlri = nlri } in

  let db, out_update = Loc_rib.update_db local_id local_asn (update, id2) db in
  assert (Prefix_map.cardinal db = 2);
  assert (List.length out_update.nlri = 0);
  assert (List.length out_update.withdrawn = 0);

  (* Test withdrawn *)
  let update = {
    withdrawn = [
      (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.19.0.0"));
      (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.20.0.0"));
    ];
    path_attrs = [];
    nlri = [];
  } in
  let db, out_update = Loc_rib.update_db local_id local_asn (update, id1) db in
  assert (Prefix_map.cardinal db = 1);
  assert (List.length out_update.nlri = 0);
  assert (List.length out_update.withdrawn = 1);
;;

let test_loc_rib_get_assoc_pfxs () = 
  let local_asn = 1_l in
  let local_id = Ipaddr.V4.of_string_exn "172.19.0.3" in

  
  (* speaker1 inserts two pfxs *)
  let id1 = Ipaddr.V4.of_string_exn "172.19.10.1" in
  let path_attrs = [
    (Bgp.Origin Bgp.EGP);
    (Bgp.As_path [Bgp.Asn_seq [5_l; 2_l]]);
    (Bgp.Next_hop id1);
  ] in
  let nlri = [ 
    (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.19.0.0"));
    (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.20.0.0"));
  ] in
  let update = { withdrawn = []; path_attrs; nlri } in
  
  let db = Prefix_map.empty in
  let db, _ = Loc_rib.update_db local_id local_asn (update, id1) db in

  (* speaker2 inserts one pfx *)
  let id2 = Ipaddr.V4.of_string_exn "172.19.10.2" in
  let path_attrs = [
    (Bgp.Origin Bgp.IGP);
    (Bgp.As_path [Bgp.Asn_seq [5_l; 2_l]]);
    (Bgp.Next_hop id2);
  ] in
  let nlri = [ (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.21.0.0")); ] in
  let update : Rib.update = { withdrawn = []; path_attrs = path_attrs; nlri = nlri } in
  
  let db, _ = Loc_rib.update_db local_id local_asn (update, id2) db in


  (* remove speaker1 *)
  let wd = Loc_rib.get_assoc_pfxes db id1 in
  let db = Loc_rib.remove_assoc_pfxes db wd in
  assert (Prefix_map.cardinal db = 1);
  assert (List.length wd = 2);
  assert (List.mem (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.19.0.0")) wd);
  assert (List.mem (Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "172.20.0.0")) wd);
;;
  

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

  let path_attrs = [
    (Bgp.Origin Bgp.EGP); 
    (Bgp.As_path [Bgp.Asn_seq [5_l; 2_l]]);
    (Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1")); 
  ] in
  let withdrawn, _ = pfxs_gen (Int32.shift_left 128_l 24) 2000 in
  let nlri, _ = pfxs_gen (Int32.shift_left 128_l 24) 2000 in
  let update = { withdrawn; path_attrs; nlri } in
  assert (List.length (Util.split_update update) = 4);
;;

let () =
  run "RIB test" [
    "Adj-RIB", [
      test_case "handle update db" `Slow test_adj_rib_update_db;
    ];
    "Loc-RIB", [
      test_case "test is_aspath_loop" `Slow test_is_aspath_loop;
      test_case "test get_aspath_len" `Slow test_get_aspath_len;
      test_case "test find_origin" `Slow test_find_origin;
      test_case "test find_aspath" `Slow test_find_aspath;
      test_case "test tie_break" `Slow test_tie_break;
      test_case "test update_db" `Slow test_loc_rib_update_db;
      test_case "test remove_assoc_pfxs" `Slow test_loc_rib_get_assoc_pfxs;
    ];
    "util", [
      test_case "test util.take" `Slow test_util_take;
      test_case "test util.split" `Slow test_util_split;
      test_case "test Util.split_update" `Slow test_split_update;
      test_case "test Util.append_aspath" `Slow test_append_aspath;
    ];
  ]
;;


