open Common
open Operators

let ip4_of_ints a b c d =
  Int32.of_int ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)

let ip6_half_of_ints a b c d = Int64.logor
  (Int64.logor (Int64.shift_left (Int64.of_int a) 48) (Int64.shift_left (Int64.of_int b) 32))
  (Int64.logor (Int64.shift_left (Int64.of_int c) 16) (Int64.of_int d))

let ip6_of_ints a b c d e f g h = ((ip6_half_of_ints a b c d), (ip6_half_of_ints e f g h))

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

(* let len_header_buffer = 19 *)

let fill_header_buffer buf len typ = 
  let marker, _ = Cstruct.split buf 16 in
  Cstruct.memset marker 0x00ff; 
  set_h_len buf len;
  set_h_typ buf (tc_to_int typ);
  sizeof_h

(* let len_open_buffer (_o: opent) =
  len_header_buffer + sizeof_opent *)

let fill_open_buffer buf (o: opent) =
  let buf_h, buf_p = Cstruct.split buf sizeof_h in
  let buf_opent, buf_opt = Cstruct.split buf_p sizeof_opent in
  let _  = fill_header_buffer buf_h (sizeof_h + sizeof_opent) OPEN in
  set_opent_version buf_opent o.version;
  set_opent_my_as buf_opent (asn_to_int o.my_as);
  set_opent_hold_time buf_opent o.hold_time;
  set_opent_bgp_id buf_opent o.bgp_id;
  set_opent_opt_len buf_opent 0;
  sizeof_h + sizeof_opent


(* TODO: Add optional parameter support *)
let gen_open_buffer (o: opent) =
  let buf = Cstruct.create 4096 in
  let len = fill_open_buffer buf o in
  let ret, _ = Cstruct.split buf len in
  ret

let gen_open_buffer2 (o: opent) =
  let buf = Cstruct.create 4096 in
  let len = fill_open_buffer buf o in
  let ret, _ = Cstruct.split buf len in
  ret

let gen_keepalive_buffer =
  let buf = Cstruct.create 19 in
  let _ = fill_header_buffer buf 19 KEEPALIVE in
  buf
  
let len_pfxs_buffer pfxs =
  let f (_, mask) = 
    let num_b = pfxlen_to_bytes mask in
    num_b + 1
  in 
    List.fold_left (+) 0 (List.map f pfxs)

let fill_pfxs_buffer buf pfxs =
  let f total_len (ip, mask) =
    let num_b = pfxlen_to_bytes mask in
    let _, buf_this = Cstruct.split buf total_len in
    Cstruct.set_uint8 buf_this 0 mask;
    (match ip with
    | Afi.IPv4 ip4 ->
      for i = 1 to num_b do
        Cstruct.set_uint8 buf_this i (Int32.to_int (ip4 >>> (32 - i * 8) &&& 0x00ff_l))
      done
    | Afi.IPv6 (hi, lo) ->
      if (num_b <= 8) then 
        for i = 1 to num_b do
          Cstruct.set_uint8 buf_this i (Int64.to_int (hi >>>> (64 - i * 8) &&&& 0x00ff_L))
        done
      else
        for i = 1 to 8 do
          Cstruct.set_uint8 buf_this i (Int64.to_int (hi >>>> (64 - i * 8) &&&& 0x00ff_L))
        done;
        for i = 9 to num_b do
          Cstruct.set_uint8 buf_this i (Int64.to_int (lo >>>> (128 - i * 8) &&&& 0x00ff_L))
        done);
    total_len + num_b + 1
  in
    (* return remaining buffer *)
    List.fold_left f 0 pfxs

let gen_pfxs_buffer pfxs = 
  let len = len_pfxs_buffer pfxs in
  let buf = Cstruct.create len in 
  let _ = fill_pfxs_buffer buf pfxs in buf

    

let attr_flags_to_uint8 {optional; transitive; partial; extlen} =
  let n_ref = ref 0 in
  if (optional) then n_ref := set_bit (!n_ref) 7 1;
  if (transitive) then n_ref := set_bit (!n_ref) 6 1;
  if (partial) then n_ref := set_bit (!n_ref) 5 1;
  if (extlen) then n_ref := set_bit (!n_ref) 4 1;
  !n_ref

let len_attr_ft_buffer = sizeof_ft

let fill_attr_ft_buffer buf flags tc len =
  set_ft_flags buf (attr_flags_to_uint8 flags);
  set_ft_tc buf (attr_to_int tc);
  set_ft_len buf len;
  sizeof_ft

let gen_attr_ft_buffer flags tc len =
  let buf = Cstruct.create 4096 in
  let len = fill_attr_ft_buffer buf flags tc len in
  let ret, _ = Cstruct.split buf len in
  ret

let gen_attr_fte_buffer flags tc len =
  let buf = Cstruct.create (sizeof_fte) in
  set_fte_flags buf (attr_flags_to_uint8 flags);
  set_fte_tc buf (attr_to_int tc);
  set_fte_len buf len;
  buf

let len_attr_fte_buffer = sizeof_fte

let fill_attr_fte_buffer buf flags tc len =
  set_fte_flags buf (attr_flags_to_uint8 flags);
  set_fte_tc buf (attr_to_int tc);
  set_fte_len buf len;
  sizeof_fte

let fill_attr_as_path_data_buffer buf asp =
  let f total_len set_or_seq = 
    let st, l = match set_or_seq with Set v -> (1, v) | Seq v -> (2, v) in
    let _, buf_this = Cstruct.split buf total_len in
    let l_len = List.length l in
    Cstruct.set_uint8 buf_this 0 st;
    Cstruct.set_uint8 buf_this 1 l_len;

    let i = ref 0 in
    let rec loop l =
      match l with
      | [] -> ()
      | x::xs -> Cstruct.BE.set_uint16 buf_this (2 + (!i) * 2) (Int32.to_int x); i := !i + 1; loop xs
    in
    loop l;
    total_len + (l_len + 1) * 2
  in
  List.fold_left f 0 asp

let gen_attr_as_path_data_buffer asp =
  let buf = Cstruct.create 4096 in
  let len = fill_attr_as_path_data_buffer buf asp in
  let ret, _ = Cstruct.split buf len in ret

let fill_path_attrs_buffer buf path_attrs =
  let flags = { optional=false; transitive=true; partial=false; extlen=false } in
  let f total_len path_attr =
    let _, buf_slice = Cstruct.split buf total_len in
    match path_attr with
    | Origin origin_opt -> 
      let n_origin = match origin_opt with
        | Some origin -> origin_to_int origin
        | None -> raise (Failure "Invalid origin")
      in
        let len_ft = fill_attr_ft_buffer buf_slice flags ORIGIN 1 in
        Cstruct.set_uint8 buf_slice len_ft n_origin;
        total_len + len_ft + 1
    | As_path asp ->
      let buf_ft, buf_p = Cstruct.split buf_slice sizeof_ft in
      let len_p = fill_attr_as_path_data_buffer buf_p asp in
      let len_ft = fill_attr_ft_buffer buf_ft flags AS_PATH len_p in
      total_len + len_ft + len_p
    | Next_hop ip4 -> 
      let buf_ft, buf_p = Cstruct.split buf_slice sizeof_ft in
      let len_ft = fill_attr_ft_buffer buf_ft flags NEXT_HOP 4 in
      Cstruct.BE.set_uint32 buf_p 0 ip4;
      total_len + len_ft + 4
    | _ -> total_len
  in
    List.fold_left f 0 path_attrs

let gen_path_attrs_buffer path_attrs =
  (* TODO: use flags from input instead of a default flag *)
  let buf = Cstruct.create 4096 in
  let len = fill_path_attrs_buffer buf path_attrs in
  let ret, _ = Cstruct.split buf len in ret


let fill_update_buffer buf { withdrawn; path_attrs; nlri } = 
  let buf_h, buf_p = Cstruct.split buf sizeof_h in
  let buf_len_wd, buf_wd_rest = Cstruct.split buf_p 2 in 
  let len_wd = fill_pfxs_buffer buf_wd_rest withdrawn in
  let _, buf_rest = Cstruct.split buf_wd_rest len_wd in
  let buf_len_pa, buf_pa_rest = Cstruct.split buf_rest 2 in
  let len_pa = fill_path_attrs_buffer buf_pa_rest path_attrs in
  let _, buf_nlri = Cstruct.split buf_pa_rest len_pa in
  let len_nlri = fill_pfxs_buffer buf_nlri nlri in
  Cstruct.BE.set_uint16 buf_len_wd 0 len_wd;
  Cstruct.BE.set_uint16 buf_len_pa 0 len_pa;
  let _ = fill_header_buffer buf_h (sizeof_h + len_wd + len_pa + len_nlri + 4) UPDATE in
  sizeof_h + len_wd + len_pa + len_nlri + 4

let gen_update_buffer u =
  let buf = Cstruct.create 4096 in
  let len = fill_update_buffer buf u in
  let ret, _ = Cstruct.split buf len in ret
  
let fill_notification_buffer buf =
  let _ = fill_header_buffer buf 21 NOTIFICATION in
  Cstruct.BE.set_uint16 buf 19 0;
  buf

let gen_notification_buffer =
  let buf = Cstruct.create 21 in
  let _ = fill_notification_buffer buf in
  buf

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
  | Some v -> Printf.printf "%s\n" (Bgp.to_string v)
  | _ -> Printf.printf "Bad\n"

let () =
  let o = {
    version=4;
    my_as= Asn 2;
    hold_time=180;
    bgp_id=1001_l;
    options=[]
  }
  in
  let buf = gen_open_buffer o in
  match Bgp.parse buf () with
  | Some v -> Printf.printf "%s\n" (Bgp.to_string v)
  | _ -> Printf.printf "Bad\n"

  
  
