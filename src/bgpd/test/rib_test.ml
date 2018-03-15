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

let test_update_aspath () =
  let attrs = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [65001_l; 65002_l]; Asn_set [65003_l]; ];
    Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.2");
  ] in
  let open Loc_rib in
  let updated = update_aspath 65000_l attrs in
  let expected = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [65000_l; 65001_l; 65002_l]; Asn_set [65003_l]; ];
    Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.2");
  ] in
  assert (updated = expected);
;;

let test_update_nexthop () =
  let id1 = Ipaddr.V4.of_string_exn "172.19.10.1" in
  let id2 = Ipaddr.V4.of_string_exn "172.19.10.2" in
  let attrs = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [65001_l; 65002_l]; Asn_set [65003_l]; ];
    Bgp.Next_hop id1;
  ] in
  let open Loc_rib in
  let updated = update_nexthop id2 attrs in
  let expected = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [65001_l; 65002_l]; Asn_set [65003_l]; ];
    Bgp.Next_hop id2;
  ] in
  assert (updated = expected);
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
  assert (Loc_rib.tie_break pa1 pa2);

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
  assert (Loc_rib.tie_break pa3 pa4 = false);

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
  assert (Loc_rib.tie_break pa5 pa6 = false);
;;

let test_adj_rib_update_db () = 
  let db = Prefix_map.empty in
  let dict = Dict.empty in

  let pfx1 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "172.19.1.0") in
  let pfx2 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "172.19.2.0") in
  let pfx3 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "172.19.3.0") in

  (* Test insertion *)
  let path_attrs = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [ 65001_l; 65002_l; 65003_l; ]];
    Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1");
  ] in
  let nlri = [ pfx1; pfx2; ] in
  let update1 = { withdrawn = []; path_attrs; nlri } in
  
  let db, dict, out_update1 = Adj_rib_in.update_in_db update1 db dict in
  let attr_id = ID.create path_attrs in

  assert (Prefix_map.cardinal db = 2);
  assert (Prefix_map.mem pfx1 db);
  assert (Prefix_map.mem pfx2 db);
  assert (Prefix_map.find pfx1 db = attr_id);
  assert (Prefix_map.find pfx2 db = attr_id);

  assert (Dict.cardinal dict = 1);
  assert (Dict.mem attr_id dict);
  assert (Dict.count attr_id dict = 2);
  assert (Dict.find attr_id dict = path_attrs);
  
  assert (List.length out_update1.nlri = 2);
  assert (out_update1.path_attrs = path_attrs);
  assert (List.length out_update1.withdrawn = 0);

  let path_attrs2 = [
    Bgp.Origin Bgp.IGP;
    Bgp.As_path [Bgp.Asn_seq [ 65004_l; 65005_l]];
    Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1");
  ] in
  let nlri2 = [ pfx3; ] in
  let update2 = { withdrawn = []; path_attrs = path_attrs2; nlri = nlri2 } in
  let db, dict, out_update2 = Adj_rib_in.update_in_db update2 db dict in

  let attr_id2 = ID.create path_attrs2 in
  assert (Prefix_map.cardinal db = 3);
  assert (Prefix_map.mem pfx3 db);
  assert (Prefix_map.find pfx3 db = attr_id2);
  
  assert (Dict.cardinal dict = 2);
  assert (Dict.mem attr_id2 dict);
  assert (Dict.count attr_id2 dict = 1);
  assert (Dict.find attr_id2 dict = path_attrs2);

  assert (List.length out_update2.nlri = 1);
  assert (out_update2.path_attrs = path_attrs2);
  assert (List.length out_update2.withdrawn = 0);

  (* Test replace *)
  let nlri3 = [ pfx1; ] in
  let update3 = { withdrawn = []; path_attrs = path_attrs2; nlri = nlri3 } in
  let db, dict, out_update3 = Adj_rib_in.update_in_db update3 db dict in
  
  assert (Prefix_map.cardinal db = 3);
  assert (Prefix_map.mem pfx1 db);
  assert (Prefix_map.find pfx1 db = attr_id2);
  
  assert (Dict.cardinal dict = 2);
  assert (Dict.count attr_id dict = 1);
  assert (Dict.count attr_id2 dict = 2);

  assert (List.length out_update3.nlri = 1);
  assert (out_update3.path_attrs = path_attrs2);
  assert (List.length out_update3.withdrawn = 0);

  (* Test withdrawn *)
  let update4 = {
    withdrawn = [ pfx2; pfx3; ];
    path_attrs = [];
    nlri = [];
  } in
  let db, dict, out_update4 = Adj_rib_in.update_in_db update4 db dict in
  
  assert (Prefix_map.cardinal db = 1);
  assert (Prefix_map.mem pfx1 db);
  assert (Prefix_map.find pfx1 db = attr_id2);

  assert (Dict.cardinal dict = 1);
  assert (Dict.mem attr_id2 dict);
  assert (Dict.count attr_id dict = 0);
  assert (Dict.count attr_id2 dict = 1);

  assert (List.length out_update4.nlri = 0);
  assert (out_update4.path_attrs = []);
  assert (List.length out_update4.withdrawn = 2);
;;

let test_loc_rib_update_db () = 
  let local_asn = 65000_l in
  let local_id = Ipaddr.V4.of_string_exn "172.19.0.3" in

  let db = Prefix_map.empty in
  let dict = Dict.empty in

  let pfx1 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.19.1.0") in
  let pfx2 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.19.2.0") in
  let pfx3 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.19.3.0") in

  let id1 = Ipaddr.V4.of_string_exn "172.19.10.1" in
  let id2 = Ipaddr.V4.of_string_exn "172.19.10.2" in

  let path_attrs1 = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [65001_l; 65002_l]];
    Bgp.Next_hop id1;
  ] in
  let expected_attrs1 = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [ local_asn; 65001_l; 65002_l ]];
    Bgp.Next_hop local_id;
  ] in
  let expected_attr_id1 = ID.create expected_attrs1 in

  let path_attrs2 = [
    Bgp.Origin Bgp.IGP;
    Bgp.As_path [Bgp.Asn_seq [65003_l; 65004_l]];
    Bgp.Next_hop id2;
  ] in
  let expected_attrs2 = [
    Bgp.Origin Bgp.IGP;
    Bgp.As_path [Bgp.Asn_seq [ local_asn; 65003_l; 65004_l]];
    Bgp.Next_hop local_id;
  ] in
  let expected_attr_id2 = ID.create expected_attrs2 in

  let path_attrs3 = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [65005_l]];
    Bgp.Next_hop id2;
  ] in
  let expected_attrs3 = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [ local_asn; 65005_l; ]];
    Bgp.Next_hop local_id;
  ] in
  let expected_attr_id3 = ID.create expected_attrs3 in

  (* Test insertion *)
  let nlri = [ pfx1 ] in
  let update = { withdrawn = []; path_attrs = path_attrs1; nlri } in
  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id1, update) db dict in
  
  assert (List.length out_update.nlri = 1);
  
  assert (out_update.path_attrs = expected_attrs1);
  assert (List.length out_update.withdrawn = 0);

  assert (Prefix_map.cardinal db = 1);
  assert (Prefix_map.mem pfx1 db);
  
  let stored_attr_id, stored_peer_id = Prefix_map.find pfx1 db in
  assert (stored_attr_id = expected_attr_id1);
  assert (stored_peer_id = id1);

  assert (Dict.cardinal dict = 1);
  assert (Dict.mem stored_attr_id dict);
  assert (Dict.count stored_attr_id dict = 1);
  assert (Dict.find stored_attr_id dict = expected_attrs1);

  let nlri = [ pfx2 ] in
  let update = { withdrawn = []; path_attrs = path_attrs2; nlri = nlri } in
  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id2, update) db dict in
    
  assert (List.length out_update.nlri = 1);
  assert (List.length out_update.withdrawn = 0);
  assert (out_update.path_attrs = expected_attrs2);

  assert (Prefix_map.cardinal db = 2);
  assert (Prefix_map.mem pfx2 db);

  assert (Dict.cardinal dict = 2);
  assert (Dict.mem expected_attr_id2 dict);
  assert (Dict.count expected_attr_id2 dict = 1);

  (* Test loop detection *)
  let path_attrs = [
    Bgp.Origin Bgp.IGP;
    Bgp.As_path [Bgp.Asn_seq [65003_l; local_asn; 65004_l]];
    Bgp.Next_hop id2;
  ] in
  let nlri = [ pfx3 ] in
  let update : Rib.update = { withdrawn = []; path_attrs = path_attrs; nlri = nlri } in
  
  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id2, update) db dict in
  assert (Prefix_map.cardinal db = 2);
  assert (List.length out_update.nlri = 0);
  assert (List.length out_update.withdrawn = 0);

  (* Test same src replace *)
  let path_attrs = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [65005_l; 65002_l; 65003_l]];
    Bgp.Next_hop id1;
  ] in
  let nlri = [ pfx1 ] in
  let update = { withdrawn = []; path_attrs; nlri } in

  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id1, update) db dict in
  
  assert (Prefix_map.cardinal db = 1);
  assert (not (Prefix_map.mem pfx1 db));

  assert (Dict.cardinal dict = 1);
  assert (not (Dict.mem expected_attr_id1 dict));

  assert (List.length out_update.nlri = 0);
  assert (out_update.withdrawn = [ pfx1 ]);

  (* This advertised route would be taken because its as_path is shorter *)
  
  let nlri = [ pfx1 ] in
  let update = { withdrawn = []; path_attrs = path_attrs3; nlri } in
  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id2, update) db dict in
  
  assert (out_update.nlri = [ pfx1 ]);
  assert (expected_attrs3 = out_update.path_attrs);
  assert (List.length out_update.withdrawn = 0);

  assert (Prefix_map.cardinal db = 2);
  assert (Prefix_map.mem pfx1 db);
  let stored_attr_id, stored_peer_id = Prefix_map.find pfx1 db in
  assert (stored_attr_id = expected_attr_id3);
  assert (stored_peer_id = id2);

  assert (Dict.cardinal dict = 2);
  assert (Dict.mem expected_attr_id3 dict);
  assert (Dict.count expected_attr_id3 dict = 1);
  assert (Dict.find expected_attr_id3 dict = expected_attrs3);

  (* This advertised route would not be chosen as its as_path is longer than the current one *)
  let path_attrs = [
    Bgp.Origin Bgp.EGP;
    Bgp.As_path [Bgp.Asn_seq [65007_l; 65008_l; 65009_l; 65010_l]];
    Bgp.Next_hop id1;
  ] in
  let nlri = [ pfx1 ] in
  let update = { withdrawn = []; path_attrs = path_attrs; nlri = nlri } in
  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id1, update) db dict in

  assert (List.length out_update.nlri = 0);
  assert (List.length out_update.withdrawn = 0);
  
  assert (Prefix_map.cardinal db = 2);
  assert (Prefix_map.mem pfx1 db);
  let stored_attr_id, stored_peer_id = Prefix_map.find pfx1 db in
  assert (stored_attr_id = expected_attr_id3);
  assert (stored_peer_id = id2);

  assert (Dict.cardinal dict = 2);

  (* Test withdrawn *)
  let update = {
    withdrawn = [ pfx1 ];
    path_attrs = [];
    nlri = [];
  } in
  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id2, update) db dict in
  assert (Prefix_map.cardinal db = 1);
  assert (not (Prefix_map.mem pfx1 db));

  assert (List.length out_update.nlri = 0);
  assert (out_update.withdrawn = [ pfx1 ]);

  assert (Dict.cardinal dict = 1);
  assert (not (Dict.mem expected_attr_id3 dict));
  assert (Dict.count expected_attr_id2 dict = 1);

  (* Test withdrawn that should not affect Loc RIB. *)
  let update = {
    withdrawn = [ pfx2 ];
    path_attrs = [];
    nlri = [];
  } in
  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id1, update) db dict in
  
  assert (Prefix_map.cardinal db = 1);
  assert (Prefix_map.mem pfx2 db);
  
  assert (List.length out_update.nlri = 0);
  assert (List.length out_update.withdrawn = 0);

  assert (Dict.cardinal dict = 1);

  (* Specific test for attr ID *)
  (* Insert *)
  let nlri = [ pfx3 ] in
  let update = { withdrawn = []; path_attrs = path_attrs2; nlri = nlri } in
  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id2, update) db dict in

  assert (Prefix_map.cardinal db = 2);
  
  assert (Dict.cardinal dict = 1);
  assert (Dict.mem expected_attr_id2 dict);
  assert (Dict.count expected_attr_id2 dict = 2);

  (* Removal *)
  let update = { withdrawn = [ pfx2; pfx3 ]; path_attrs = []; nlri = [] } in
  let db, dict, out_update = Loc_rib.update_loc_db local_id local_asn (id2, update) db dict in

  assert (Prefix_map.cardinal db = 0);
  assert (Dict.cardinal dict = 0);
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

let test_take () =
  let pfxs = [
    Ipaddr.V4.Prefix.make 8 (Ipaddr.V4.of_string_exn "10.0.0.0");
    Ipaddr.V4.Prefix.make 16 (Ipaddr.V4.of_string_exn "66.173.0.0");
    Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "172.168.13.0");
  ] in
  let taken, rest = Adj_rib_out.take pfxs 1 in
  assert (List.length taken = 1);
  assert (List.length rest = 2);
;;

let test_split () =
  let pfxs, _ = pfxs_gen (Int32.shift_left 128_l 24) 1000 in
  let split, rest = Adj_rib_out.split pfxs 400 in
  assert (List.length split = 2);
  assert (List.length rest = 200);

  let pfxs, _ = pfxs_gen (Int32.shift_left 128_l 24) 2000 in
  let split, rest = Adj_rib_out.split pfxs 400 in
  assert (List.length split = 5);
  assert (List.length rest = 0);
;;

let test_out_rib_build_db () =
  let pfx1 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.10.13.0") in
  let pfx2 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.10.14.0") in
  let pfx3 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.10.15.0") in
  let pfx4 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.10.16.0") in
  let pfx5 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.10.17.0") in
  let pfx6 = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_string_exn "10.10.18.0") in
  
  (* pfx1: withdrawn-after-advertisement *)
  (* pfx2: advertisement-after-advertisement *)
  (* pfx3: advertisement-after-withdrawn *)
  (* pfx4: only get withdrawn *)
  (* pfx5: only get advertised *)
  (* pfx6: withdrawn after withdrawn *)

  let attrs1 = [
    Bgp.Origin Bgp.EGP; 
    Bgp.As_path [Bgp.Asn_seq [65001_l; 65002_l; 65003_l]];
    Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1"); 
  ] in
  let wd1 = [ pfx1; pfx6 ] in
  let nlri1 = [ pfx2; pfx3; ] in
  let u1 = { withdrawn = wd1; path_attrs = attrs1; nlri = nlri1 } in

  let attrs2 = [
    Bgp.Origin Bgp.EGP; 
    Bgp.As_path [Bgp.Asn_seq [65001_l; 65002_l; 65003_l; 65004_l]];
    Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1"); 
  ] in
  let wd2 = [ pfx3; pfx4 ] in
  let nlri2 = [ pfx1; pfx2; ] in
  let u2 = { withdrawn = wd2; path_attrs = attrs2; nlri = nlri2 } in

  let attrs3 = [
    Bgp.Origin Bgp.IGP; 
    Bgp.As_path [Bgp.Asn_seq [65001_l; 65007_l; 65004_l]];
    Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1"); 
  ] in
  let wd3 = [ pfx6 ] in
  let nlri3 = [ pfx5 ] in
  let u3 = { withdrawn = wd3; path_attrs = attrs3; nlri = nlri3 } in

  let rev_updates = [u1; u2; u3] in
  let open Adj_rib_out in
  let wd, ins, db = build_db rev_updates in

  assert (Prefix_set.cardinal wd = 3);
  assert (Prefix_set.mem pfx1 wd);
  assert (Prefix_set.mem pfx4 wd);
  assert (Prefix_set.mem pfx6 wd);

  assert (Prefix_set.cardinal ins = 3);
  assert (Prefix_set.mem pfx2 ins);
  assert (Prefix_set.mem pfx3 ins);
  assert (Prefix_set.mem pfx5 ins);

  assert (ID_map.cardinal db = 2);
  assert (ID_map.mem (ID.create attrs1) db);
  assert (ID_map.mem (ID.create attrs3) db);
  let _attrs, ins = ID_map.find (ID.create attrs1) db in
  assert (List.length ins = 2);
  assert (List.mem pfx2 ins);
  assert (List.mem pfx3 ins);
  let _attrs, ins2 = ID_map.find (ID.create attrs3) db in
  assert (List.length ins2 = 1);
  assert (List.mem pfx5 ins2);
;;


let test_util_update_nexthop () =
  let path_attrs = [
    (Bgp.Origin Bgp.EGP); 
    (Bgp.As_path [Bgp.Asn_seq [5_l; 2_l]]);
    (Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1")); 
  ] in
  assert ((find_next_hop path_attrs) = Some (Ipaddr.V4.of_string_exn "172.19.10.1"));

  let updated = Loc_rib.update_nexthop (Ipaddr.V4.of_string_exn "172.19.10.3") path_attrs in
  assert ((find_next_hop updated) = Some (Ipaddr.V4.of_string_exn "172.19.10.3"));
;;

let test_util_update_aspath () =
  let path_attrs = [
    (Bgp.Origin Bgp.EGP); 
    (Bgp.As_path [Bgp.Asn_seq [5_l; 2_l]]);
    (Bgp.Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1")); 
  ] in
  assert ((find_aspath path_attrs) = Some ([Asn_seq [5_l; 2_l]]));

  let updated = Loc_rib.update_aspath 1_l path_attrs in
  assert (find_aspath updated = Some ([Asn_seq [1_l; 5_l; 2_l]]));
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
      test_case "test update aspath" `Slow test_update_aspath;
      test_case "test update nexthop" `Slow test_update_nexthop;
      test_case "test tie_break" `Slow test_tie_break;
      test_case "test update_db" `Slow test_loc_rib_update_db;
    ];
    "Out RIB", [
      test_case "test take" `Slow test_take;
      test_case "test split" `Slow test_split;
      test_case "test build db" `Slow test_out_rib_build_db;
    ];
    "util", [
    ];
  ]
;;


