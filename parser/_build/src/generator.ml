[%%cenum
  type tc =
    | OPEN [@id 1]
    | UPDATE
    | NOTIFICATION
    | KEEPALIVE
  [@@uint8_t]
]

[%%cstruct
  type h = {
    marker: uint8_t [@len 16];
    len: uint16_t;
    typ: uint8_t;
  }
  [@@big_endian]
]

[%%cstruct
  type opent = {
    version: uint8_t;
    my_as: uint16_t;
    hold_time: uint16_t;
    bgp_id: uint32_t;
    opt_len: uint8_t;
  }
  [@@big_endian]
]

let gen_header len typ = 
  let buf = Cstruct.create 19 in
  set_h_len buf len;
  set_h_typ buf (tc_to_int typ);
  buf

let gen_open (o: Bgp.opent) =
  let total_length = sizeof_h + sizeof_opent in
  let h = gen_header total_length OPEN in
  let p = Cstruct.create (sizeof_opent) in
  set_opent_version p o.version;
  set_opent_my_as p o.my_as;
  set_opent_hold_time p o.hold_time;
  set_opent_bgp_id p o.bgp_id;
  set_opent_opt_len p 0;
  Cstruct.concat [h; p]

let gen_keepalive =
  gen_header 19 NOTIFICATION




let () =
  let buf = gen_open 4 2 180 (Int32.of_int 378) 0 in
  let parsed = Bgp.parse buf () in
  match parsed with
  | Some v -> Printf.printf "%s" (Bgp.to_string v)
  | _ -> Printf.printf "Bad"
