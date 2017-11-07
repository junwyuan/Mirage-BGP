(*
 * Copyright (c) 2012-2017 Richard Mortier <mort@cantab.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf
open Operators

(* Lame, lame, lame. RFC6396, sec. 4.3.4 says that AS_PATHs MUST be encoded as
   4 bytes in a TABLE_DUMP_V2 RIB_ENTRY, no matter what. Similarly in BGP4MP
   MESSAGE_AS4 and LOCAL_AS4 message types (4.4.3 and 4.4.6). The first
   section (4.3.4) then says, in the same section, that MP_REACH_NLRI
   attributes contain only the nexthop address length and address, not the
   AFI, SAFI and NLRI fields as these are encoded in the RIB entry header.

   We hack around this via the `caller` parameter, typed appropriately; and by
   forcing aspath to always contain `int32`.

   MRT remains IMO, in random ways, a half-witted format. *)

type caller = Normal | Table2 | Bgp4mp_as4

type asn = Asn of int | Asn4 of int32
let asn_to_string = function
  | Asn a -> sprintf "%d" a
  | Asn4 a ->
    if a < 65536_l then sprintf "%ld" a
    else
      sprintf "%ld.%ld" (a >>> 16) (a &&& 0xFFFF_l)

let pfxlen_to_bytes l = (l+7) / 8

let get_nlri4 buf off =
  Cstruct.(
    let v = ref 0l in
    let pl = get_uint8 buf off in
    let bl = pfxlen_to_bytes pl in
    for i = 0 to bl-1 do
      v := (!v <<< 8) +++ (Int32.of_int (get_uint8 buf (off+i+1)))
    done;
    Afi.IPv4 (!v <<< (8*(4 - bl))), pl
  )

let get_nlri6 buf off =
  Cstruct.(
    let pl = get_uint8 buf off in
    let bl = pfxlen_to_bytes pl in
    let hi =
      let v = ref 0L in
      let n = min 7 (bl-1) in
      for i = 0 to n do
        v := (!v <<<< 8) ++++ (Int64.of_int (get_uint8 buf off+i+1))
      done;
      !v <<<< (8*(8 - n))
    in
    let lo =
      let v = ref 0L in
      let n = min 15 (bl-1) in
      for i = 8 to n do
        v := (!v <<<< 8) ++++ (Int64.of_int (get_uint8 buf off+i+1))
      done;
      !v <<<< (8*(8 - n))
    in
    Afi.IPv6 (hi, lo), pl
  )

let get_partial buf =
  let get_partial_ip4 buf =
    Cstruct.(
      let v = ref 0l in
      for i = 0 to (min 3 ((len buf)-1)) do
        v := (!v <<< 8) +++ (Int32.of_int (get_uint8 buf i))
      done;
      !v <<< (8*(4 - len buf))
    )
  in
  let get_partial_ip6 buf =
    Cstruct.(
      let hi =
        let v = ref 0L in
        let n = min 7 ((len buf)-1) in
        for i = 0 to n do
          v := (!v <<<< 8) ++++ (Int64.of_int (get_uint8 buf i))
        done;
        !v <<<< (8*(8 - n))
      in
      let lo =
        let v = ref 0L in
        let n = min 15 ((len buf)-1) in
        for i = 8 to n do
          v := (!v <<<< 8) ++++ (Int64.of_int (get_uint8 buf i))
        done;
        !v <<<< (8*(8 - n))
      in
      hi, lo
    )
  in
  let l = Cstruct.get_uint8 buf 0 in
  let bl = pfxlen_to_bytes l in
  let ip,bs = Cstruct.split ~start:1 buf bl in
  let ip =
    if bl > 4 then
      let (hi,lo) = get_partial_ip6 ip in Afi.IPv6 (hi,lo)
    else
      Afi.IPv4 (get_partial_ip4 ip)
  in (ip,l)

let parse_nlris buf =
  let lenf buf = Some (1 + (pfxlen_to_bytes (Cstruct.get_uint8 buf 0))) in
  let pf buf =
    if pfxlen_to_bytes (Cstruct.get_uint8 buf 0) <= 4 then
      get_nlri4 buf 0
    else
      get_nlri6 buf 0
  in
  Cstruct.iter lenf pf buf

[%%cstruct
  type h = {
     marker: uint8_t; [@len 16]
     len: uint16_t;
     typ: uint8_t;
   }
  [@@big_endian]
]

[%%cenum
  type tc =
    | OPEN [@id 1]
    | UPDATE
    | NOTIFICATION
    | KEEPALIVE
  [@@uint8_t]
]

[%%cenum
  type cc =
    | MP_EXT                      [@id 1]
    | ROUTE_REFRESH
    | OUTBOUND_ROUTE_FILTERING
    | MULTIPLE_ROUTES_DESTINATION
    | EXT_HEXTHOP_ENC
    | GRACEFUL_RESTART            [@id 64]
    | AS4_SUPPORT
    | ENHANCED_REFRESH            [@id 70]
  [@@uint8_t]
]

[%%cstruct
  type mp_ext = {
    afi: uint16_t;
    safi: uint16_t;
  }
  [@@big_endian]
]

type capability =
  | Mp_ext of Afi.tc * Safi.tc
  | Ecapability of Cstruct.t

let capability_to_string = function
  | Mp_ext (a,s) ->
    sprintf "MP_EXT(%s,%s)" (Afi.tc_to_string a) (Safi.tc_to_string s)
  | Ecapability _ -> "UNKNOWN_CAPABILITY"

let parse_capability buf = function
  | Some MP_EXT -> Mp_ext (get_mp_ext_afi buf |> Afi.int_to_tc,
                           get_mp_ext_safi buf |> Safi.int_to_tc)
  | Some ROUTE_REFRESH
  | Some OUTBOUND_ROUTE_FILTERING
  | Some MULTIPLE_ROUTES_DESTINATION
  | Some EXT_HEXTHOP_ENC
  | Some GRACEFUL_RESTART
  | Some AS4_SUPPORT
  | Some ENHANCED_REFRESH
  | None
    -> Ecapability buf

[%%cenum
  type oc =
    | RESERVED [@id 0]
    | AUTHENTICATION
    | CAPABILITY
  [@@uint8_t]
]

type opt_param =
  | Reserved (* wtf? *)
  | Authentication (* deprecated, rfc 4271 *)
  | Capability of capability

let opt_param_to_string = function
  | Reserved -> "RESERVED"
  | Authentication -> "AUTH"
  | Capability c -> sprintf "CAP(%s)" (capability_to_string c)

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

type opent = {
  version: int;
  my_as: asn;
  hold_time: int;
  bgp_id: int32;
  options: opt_param list;
}

let opent_to_string o =
  sprintf "version:%d, my_as:%s, hold_time:%d, bgp_id:0x%08lx, options:[%s]"
    o.version (asn_to_string o.my_as) o.hold_time o.bgp_id
    (o.options ||> opt_param_to_string |> String.concat "; ")

[%%cenum
  type attr =
    | ORIGIN [@id 1]
    | AS_PATH
    | NEXT_HOP
    | MED
    | LOCAL_PREF
    | ATOMIC_AGGR
    | AGGREGATOR
    | COMMUNITY
    | MP_REACH_NLRI [@id 14]
    | MP_UNREACH_NLRI
    | EXT_COMMUNITIES
    | AS4_PATH
  [@@uint8_t]
]

[%%cenum
  type origin =
    | IGP
    | EGP
    | INCOMPLETE
  [@@uint8_t]
]

[%%cstruct
  type ft = {
    flags: uint8_t;
    tc: uint8_t;
    len: uint8_t;
  }
  [@@big_endian]
]

[%%cstruct
  type fte = {
    flags: uint8_t;
    tc: uint8_t;
    len: uint16_t
  }
  [@@big_endian]
]

let is_optional f = is_bit 7 f
let is_transitive f = is_bit 6 f
let is_partial f = is_bit 5 f
let is_extlen f = is_bit 4 f

[%%cenum
  type aspt =
    | AS_SET [@id 1]
    | AS_SEQ
  [@@uint8_t]
]

[%%cstruct
  type asp = {
    t: uint8_t;
    n: uint8_t;
  }
  [@@big_endian]
]

type asp = Set of int32 Cstruct.iter | Seq of int32 Cstruct.iter
let parse_as4path buf =
  let lenf buf = Some (sizeof_asp + get_asp_n buf*4) in
  let pf buf =
    let t = get_asp_t buf in
    let buf = Cstruct.shift buf sizeof_asp in
    let vs = Cstruct.iter
        (fun buf -> Some 4)
        (fun buf -> Cstruct.BE.get_uint32 buf 0)
        buf
    in
    match int_to_aspt t with
    | None -> failwith "parse_as4path: unknown segment type"
    | Some AS_SET -> Set vs
    | Some AS_SEQ -> Seq vs
  in
  Cstruct.iter lenf pf buf

let aspath_to_string a =
  let rec asps_to_string a = match a () with
    | None -> ""
    | Some v -> sprintf "%ld <- %s" v (asps_to_string a)
  in match a () with
  | None -> ""
  | Some Set v -> sprintf "set(%s)" (asps_to_string v)
  | Some Seq v -> sprintf "seq(%s)" (asps_to_string v)

let parse_aspath buf =
  let lenf buf = Some (sizeof_asp + get_asp_n buf * 2) in
  let pf buf =
    let t = get_asp_t buf in
    let buf = Cstruct.shift buf sizeof_asp in
    let vs = Cstruct.iter
        (fun buf -> Some 2)
        (fun buf -> Cstruct.BE.get_uint16 buf 0 |> Int32.of_int)
        buf
    in
    match int_to_aspt t with
    | None -> failwith "parse_aspath: unknown segment type"
    | Some AS_SET -> Set vs
    | Some AS_SEQ -> Seq vs
  in
  Cstruct.iter lenf pf buf

type path_attr =
  | Origin of origin option
  | As_path of asp Cstruct.iter
  | Next_hop of Afi.ip4
  | Community of int32
  | Ext_communities
  | Med of int32
  | Atomic_aggr
  | Aggregator
  | Mp_reach_nlri
  | Mp_unreach_nlri
  | As4_path of asp Cstruct.iter

type path_attrs = path_attr Cstruct.iter

let parse_path_attrs ?(caller=Normal) buf =
  let lenf buf =
    let f = get_ft_flags buf in
    Some (if is_extlen f then
            sizeof_fte + get_fte_len buf
          else
            sizeof_ft + get_ft_len buf
         )
  in
  let pf buf =
    let hlen =
      if buf |> get_ft_flags |> is_extlen then sizeof_fte else sizeof_ft
    in
    let h,p = Cstruct.split buf hlen in
    match h |> get_ft_tc |> int_to_attr with
    | Some ORIGIN -> Origin (Cstruct.get_uint8 p 0 |> int_to_origin)
    | Some AS_PATH -> (match caller with
        | Normal -> As_path (parse_aspath p)
        | Table2 | Bgp4mp_as4 -> As4_path (parse_as4path p)
      )
    | Some AS4_PATH -> As4_path (parse_as4path p)
    | Some NEXT_HOP -> Next_hop (Cstruct.BE.get_uint32 p 0)
    | Some COMMUNITY -> Community (Cstruct.BE.get_uint32 p 0)
    | Some EXT_COMMUNITIES -> Ext_communities
    | Some MED -> Med (Cstruct.BE.get_uint32 p 0)
    | Some ATOMIC_AGGR -> Atomic_aggr
    | Some AGGREGATOR -> Aggregator
    | Some MP_REACH_NLRI -> Mp_reach_nlri
    | Some MP_UNREACH_NLRI -> Mp_unreach_nlri
    | Some LOCAL_PREF
    | None
      ->
      printf "U %d %d\n%!" (get_ft_tc h) (Cstruct.len p);
      Cstruct.hexdump p; failwith "unknown path attr"

  in
  Cstruct.iter lenf pf buf

type update = {
  withdrawn: Afi.prefix Cstruct.iter;
  path_attrs: path_attr Cstruct.iter;
  nlri: Afi.prefix Cstruct.iter;
}

let rec path_attrs_to_string iter = match iter () with
  | None -> ""
  | Some Origin v ->
    sprintf "ORIGIN(%s); %s"
      (match v with None -> "error" | Some v -> origin_to_string v)
      (path_attrs_to_string iter)
  | Some As_path v ->
    sprintf "AS_PATH(%s); %s"
      (aspath_to_string v) (path_attrs_to_string iter)
  | Some As4_path v ->
    sprintf "AS4_PATH(%s); %s"
      (aspath_to_string v) (path_attrs_to_string iter)
  | Some Next_hop v ->
    sprintf "NEXT_HOP(%s); %s"
      (Afi.ip4_to_string v) (path_attrs_to_string iter)
  | Some Community v ->
    sprintf "COMMUNITY(%ld:%ld); %s"
      (v >>> 16 &&& 0xffff_l) (v &&& 0xffff_l) (path_attrs_to_string iter)
  | Some Ext_communities -> "EXT_COMMUNITIES; " ^ (path_attrs_to_string iter)
  | Some Med v -> sprintf "MED(%ld); %s" v (path_attrs_to_string iter)
  | Some Atomic_aggr -> "ATOMIC_AGGR; " ^ (path_attrs_to_string iter)
  | Some Aggregator -> "AGGREGATOR; " ^ (path_attrs_to_string iter)
  | Some Mp_reach_nlri -> "MP_REACH_NLRI; " ^ (path_attrs_to_string iter)
  | Some Mp_unreach_nlri -> "MP_UNREACH_NLRI; " ^ (path_attrs_to_string iter)

let rec nlris_to_string iter = match iter () with
  | None -> ""
  | Some p -> (Afi.prefix_to_string p) ^ "; " ^ (nlris_to_string iter)

let update_to_string u =
  sprintf "withdrawn:[%s], path_attrs:[%s], nlri:[%s]"
    (nlris_to_string u.withdrawn)
    (path_attrs_to_string u.path_attrs)
    (nlris_to_string u.nlri)

type t =
  | Open of opent
  | Update of update
  | Notification
  | Keepalive

let to_string = function
  | Open o -> sprintf "OPEN(%s)" (opent_to_string o)
  | Update u -> sprintf "UPDATE(%s)" (update_to_string u)
  | Notification -> "NOTIFICATION"
  | Keepalive -> "KEEPALIVE"

let parse ?(caller=Normal) buf =
  let lenf buf = Some (get_h_len buf) in
  let pf buf =
    let hlen = sizeof_h in
    let h,p = Cstruct.split buf hlen in
    match get_h_typ h |> int_to_tc with
    | None -> failwith "pf: bad BGP packet"
    | Some OPEN ->
      let m,opts = Cstruct.split p (Cstruct.len p - get_opent_opt_len p) in
      let opts =
        let rec aux acc bs =
          if Cstruct.len bs = 0 then acc else (
            let t, opt, bs = Tlv.get_tlv bs in
            let opt = match int_to_oc t with
              | None -> failwith "bad option"
              | Some RESERVED -> Reserved
              | Some AUTHENTICATION -> Authentication
              | Some CAPABILITY ->
                let t,c, _ = Tlv.get_tlv bs in
                Capability (parse_capability c (int_to_cc t))
            in aux (opt :: acc) bs
          )
        in aux [] opts
      in
      Open { version = get_opent_version m;
             my_as = Asn (get_opent_my_as m);
             hold_time = get_opent_hold_time m;
             bgp_id = get_opent_bgp_id m;
             options = opts;
           }

    | Some UPDATE ->
      let withdrawn,bs =
        let wl = Cstruct.BE.get_uint16 p 0 in
        Cstruct.split ~start:2 p wl
      in
      let path_attrs,nlri =
        let pl = Cstruct.BE.get_uint16 bs 0 in
        Cstruct.split ~start:2 bs pl
      in
      Update {
        withdrawn = parse_nlris withdrawn;
        path_attrs = parse_path_attrs ~caller path_attrs;
        nlri = parse_nlris nlri;
      }

    | Some NOTIFICATION -> Notification
    | Some KEEPALIVE -> Keepalive
  in
  Cstruct.iter lenf pf buf
