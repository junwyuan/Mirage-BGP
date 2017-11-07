open Common
open Operators

let ip4_of_ints a b c d =
  Int32.of_int ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)

let gen_uint16_buffer n =
  Cstruct.(
    let buf = create 2 in
    BE.set_uint16 buf 0 n;
    buf
  )

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

let gen_header_buffer len typ = 
  let buf = Cstruct.create 19 in
  for i = 0 to 15 do
    Cstruct.set_uint8 buf i 0x00ff
  done;
  set_h_len buf len;
  set_h_typ buf (tc_to_int typ);
  buf

(* TODO: Add optional parameter support *)
let gen_open_buffer (o: opent) =
  let msg_length = sizeof_h + sizeof_opent in
  let h = gen_header_buffer msg_length OPEN in
  let p = Cstruct.create (sizeof_opent) in
  set_opent_version p o.version;
  set_opent_my_as p (asn_to_int o.my_as);
  set_opent_hold_time p o.hold_time;
  set_opent_bgp_id p o.bgp_id;
  set_opent_opt_len p 0;
  Cstruct.concat [h; p]

let gen_keepalive_buffer =
  gen_header_buffer 19 NOTIFICATION



let gen_pfxs_buffer pfxs =
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
    Cstruct.concat (List.map f pfxs)
    

let attr_flags_to_uint8 {optional; transitive; partial; extlen} =
  let n_ref = ref 0 in
  if (optional) then n_ref := set_bit (!n_ref) 7 1;
  if (transitive) then n_ref := set_bit (!n_ref) 6 1;
  if (partial) then n_ref := set_bit (!n_ref) 5 1;
  if (extlen) then n_ref := set_bit (!n_ref) 4 1;
  !n_ref

let gen_attr_ft_buffer flags tc len =
  let buf = Cstruct.create (sizeof_ft) in
  set_ft_flags buf (attr_flags_to_uint8 flags);
  set_ft_tc buf (attr_to_int tc);
  set_ft_len buf len;
  buf

let gen_attr_fte_buffer flags tc len =
  let buf = Cstruct.create (sizeof_fte) in
  set_fte_flags buf (attr_flags_to_uint8 flags);
  set_fte_tc buf (attr_to_int tc);
  set_fte_len buf len;
  buf

let gen_attr_as_path_data_buffer asp =
  let f set_or_seq = 
    let st, l = match set_or_seq with Set v -> (1, v) | Seq v -> (2, v) in
    let buf = Cstruct.create ((List.length l + 1) * 2) in
    Cstruct.set_uint8 buf 0 st;
    Cstruct.set_uint8 buf 1 (List.length l);
    let i = ref 0 in
    let rec loop l =
      match l with
      | [] -> ()
      | x::xs -> Cstruct.BE.set_uint16 buf (2 + (!i) * 2) (Int32.to_int x); i := !i + 1; loop xs
    in
    loop l; buf
  in
  Cstruct.concat (List.map f asp)

let gen_path_attrs_buffer path_attrs =
  (* TODO: use flags from input instead of a default flag *)
  let flags = { optional=false; transitive=true; partial=false; extlen=false } in
  let f = function
  | Origin origin_opt -> 
    let n_origin = match origin_opt with
      | Some origin -> origin_to_int origin
      | None -> raise (Failure "Invalid origin")
    in
      let ft = gen_attr_ft_buffer flags ORIGIN 1 in 
      let data = Cstruct.create 1 in 
      Cstruct.set_uint8 data 0 n_origin;
      Cstruct.concat [ft; data]
  | As_path asp ->
    let data = gen_attr_as_path_data_buffer asp in
    let ft = gen_attr_ft_buffer flags AS_PATH (Cstruct.len data) in
    Cstruct.concat [ft; data]
  | Next_hop ip4 -> 
    let ft = gen_attr_ft_buffer flags NEXT_HOP 4 in
    let data = Cstruct.create 4 in
    Cstruct.BE.set_uint32 data 0 ip4;
    Cstruct.concat [ft; data]
  | _ -> Cstruct.create 0
  in
  Cstruct.concat (List.map f path_attrs)

let gen_update_buffer { withdrawn; path_attrs; nlri } = 
  let buf_wd = gen_pfxs_buffer withdrawn in
  let wd_len = gen_uint16_buffer (Cstruct.len buf_wd) in
  let buf_pa = gen_path_attrs_buffer path_attrs in
  let pa_len = gen_uint16_buffer (Cstruct.len buf_pa) in
  let buf_nlri = gen_pfxs_buffer nlri in
  let h = gen_header_buffer 
    (Cstruct.len buf_wd + Cstruct.len buf_pa + Cstruct.len buf_nlri + 23) 
    UPDATE
  in
    Cstruct.concat [h; wd_len; buf_wd; pa_len; buf_pa; buf_nlri]
  
let gen_notification_buffer =
  let h = gen_header_buffer 21 NOTIFICATION in
  let p = Cstruct.create 2 in
  Cstruct.BE.set_uint16 p 0 0;
  Cstruct.concat [h; p]

let () =
  let withdrawn = 
    [(Afi.IPv4 (ip4_of_ints 192 168 0 0), 16); 
      (Afi.IPv4 (ip4_of_ints 10 0 0 0), 8); 
      (Afi.IPv4 (ip4_of_ints 172 16 84 0), 24);
      ] 
  in
  let nlri = [(Afi.IPv4 (ip4_of_ints 192 168 0 0), 24)] in
  let path_attrs = [
    Origin (Some IGP);
    As_path [Set [2_l; 5_l; 3_l]];
    Next_hop (ip4_of_ints 192 168 1 253);
  ]
  in 
  let u = {withdrawn; path_attrs; nlri} in
  let msg = gen_update_buffer u in
  match Bgp.parse msg () with
  | Some v -> Printf.printf "%s" (Bgp.to_string v)
  | _ -> Printf.printf "Bad"
  
  
