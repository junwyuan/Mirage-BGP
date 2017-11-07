open Common
open Operators

let set_bit n pos b =
  if (n > 255) then raise (Failure "Invalid argument: n is too large.")
  else if (pos > 7) then raise (Failure "Invalid argument: pos is too large.")
  else
    let n_32 = Int32.of_int n in 
    let res_32 = 
      match b with
      | 0 -> (n_32 ^^^ (1_l <<< pos))
      | 1 -> (n_32 ||| (1_l <<< pos))
      | _ -> raise (Failure "Invalid argument: b should be either 0 or 1.")
    in
      Int32.to_int res_32

let gen_header len typ = 
  let buf = Cstruct.create 19 in
  for i = 0 to 15 do
    Cstruct.set_uint8 buf i 0x00ff
  done;
  set_h_len buf len;
  set_h_typ buf (tc_to_int typ);
  buf

(* TODO: Add optional parameter support *)
let gen_open (o: opent) =
  let msg_length = sizeof_h + sizeof_opent in
  let h = gen_header msg_length OPEN in
  let p = Cstruct.create (sizeof_opent) in
  set_opent_version p o.version;
  set_opent_my_as p (asn_to_int o.my_as);
  set_opent_hold_time p o.hold_time;
  set_opent_bgp_id p o.bgp_id;
  set_opent_opt_len p 0;
  Cstruct.concat [h; p]

let gen_keepalive =
  gen_header 19 NOTIFICATION

let rec iter_map iter f =
  match iter () with
  | None -> []
  | Some v -> (f v) :: (iter_map iter f)

let gen_pfxs withdrawn =
  let f (ip, mask) = 
    let num_b = pfxlen_to_bytes mask in
    let buf = Cstruct.create (num_b + 1) in
    Cstruct.set_uint8 buf 0 mask;
    match ip with
    | Afi.IPv4 ip4 ->
      for i = 1 to num_b do
        Cstruct.set_uint8 buf i (Int32.to_int (ip4 >>> (32 - i * 8) &&& 0x00ff_l))
      done;
      buf
    | Afi.IPv6 (hi, lo) ->
      if (num_b <= 8) then 
        for i = 1 to num_b do
          Cstruct.set_uint8 buf i (Int64.to_int (hi >>>> (64 - i * 8) &&&& 0x00ff_L))
        done
      else
        for i = 1 to 8 do
          Cstruct.set_uint8 buf i (Int64.to_int (hi >>>> (64 - i * 8) &&&& 0x00ff_L))
        done;
        for i = 9 to num_b do
          Cstruct.set_uint8 buf i (Int64.to_int (lo >>>> (128 - i * 8) &&&& 0x00ff_L))
        done;
      buf
  in
    let pfxs = Cstruct.concat (iter_map withdrawn f) in
    let buf_pl = Cstruct.create 2 in
    Cstruct.BE.set_uint16 buf_pl 0 (Cstruct.len pfxs);
    Cstruct.concat [buf_pl; pfxs]

let attr_flags_to_uint8 {optional; transitive; partial; extlen} =
  let n_ref = ref 0 in
  if (optional) then n_ref := set_bit (!n_ref) 7 1;
  if (transitive) then n_ref := set_bit (!n_ref) 6 1;
  if (partial) then n_ref := set_bit (!n_ref) 5 1;
  if (extlen) then n_ref := set_bit (!n_ref) 4 1;
  !n_ref

let gen_ft flags tc len =
  let buf = Cstruct.create (sizeof_ft) in
  set_ft_flags buf (attr_flags_to_uint8 flags);
  set_ft_tc buf (attr_to_int tc);
  set_ft_len buf len;
  buf

let gen_fte flags tc len =
  let buf = Cstruct.create (sizeof_fte) in
  set_fte_flags buf (attr_flags_to_uint8 flags);
  set_fte_tc buf (attr_to_int tc);
  set_fte_len buf len;
  buf

let gen_as_path asp =
  let f set_or_seq = 
    let st, iter = match set_or_seq with Set v -> (0, v) | Seq v -> (1, v) in
    let l = iter_map iter (fun x -> x) in
    let buf = Cstruct.create ((List.length l + 1) * 2) in
    Cstruct.set_uint8 buf 0 st;
    Cstruct.set_uint8 buf 1 (List.length l);

    let rec loop l =
      let i = ref 0 in
      match l with
      | [] -> ()
      | x::xs -> Cstruct.BE.set_uint16 buf (2 + (!i) * 2) (Int32.to_int x); i := !i + 1; loop xs
    in
    loop l; buf
  in
  Cstruct.concat (iter_map asp f)

let gen_path_attrs path_attrs =
  (* TODO: use flags from input instead of a default flag *)
  let flags = { optional=false; transitive=true; partial=false; extlen=false } in
  let f = function
  | Origin origin_opt -> 
    let n_origin = match origin_opt with
      | Some origin -> origin_to_int origin
      | None -> raise (Failure "Invalid origin")
    in
      let buf_ft = gen_ft flags ORIGIN 1 in 
      let p = Cstruct.create 1 in 
      Cstruct.set_uint8 p 0 n_origin;
      Cstruct.concat [buf_ft; p]
  | As_path asp ->
    let p = gen_as_path asp in
    let buf_ft = gen_ft flags AS_PATH (Cstruct.len p) in
    Cstruct.concat [p; buf_ft]
  | Next_hop ip4 -> 
    let buf_ft = gen_ft flags NEXT_HOP 7 in
    let p = Cstruct.create 4 in
    Cstruct.BE.set_uint32 p 0 ip4;
    Cstruct.concat [buf_ft; p]
  | _ -> Cstruct.create 0
  in
  Cstruct.concat (iter_map path_attrs f)


let gen_update {withdrawn; path_attrs; nlri} = 
  let buf_wd = gen_pfxs withdrawn in
  let buf_pa = gen_path_attrs path_attrs in
  let buf_nlri = gen_pfxs nlri in
  let h = gen_header 
    (Cstruct.len buf_wd + Cstruct.len buf_pa + Cstruct.len buf_nlri + 23) 
    UPDATE
  in
    Cstruct.concat [h; buf_wd; buf_pa; buf_nlri]
  
let gen_notification =
  let h = gen_header 21 NOTIFICATION in
  Cstruct.BE.set_uint16 h 19 0;
  h 

let () =
  let ip_list = [(172168_l, 16); (10_l, 8); (192168001_l, 24)] in
  let pfx_list = List.map (fun (ip4, mask) -> 
    Cstruct.(
      let buf = create (mask + 1) in
      set_uint8 buf 0 mask;
      for i = 1 to mask do
        set_uint8 buf i (Int32.to_int (ip4 >>> (32 - 8 * i) &&& 0x00ff_l))
      done;
      buf
    )
  ) ip_list
  in 
  let withdrawn_raw = Cstruct.concat pfx_list in
  let withdrawn = Bgp.parse_nlris withdrawn_raw in
  let origin = Origin (Some IGP) in
  let next_hop = Next_hop (1721684163_l) in
  let asn_list = [2_l; 5_l; 3_l] in
  let asn_buf = List.map (fun x -> 
    let buf = Cstruct.create 4 in
    Cstruct.BE.set_uint32 buf 0 x;
    buf
    ) asn_list
  in
  let h = Cstruct.create 2 in
  Cstruct.set_uint8 h 0 1;
  Cstruct.set_uint8 h 1 3;
  let asp = parse_aspath (Cstruct.concat (h :: asn_buf)) in
  let h2 = Cstruct.create 2 in
  set_asp_t h2 1;
  set_asp_n h2 3;
  
 

    






(* let () =
  let buf = gen_open 4 2 180 (Int32.of_int 378) 0 in
  let parsed = Bgp.parse buf () in
  match parsed with
  | Some v -> Printf.printf "%s" (Bgp.to_string v)
  | _ -> Printf.printf "Bad" *)
