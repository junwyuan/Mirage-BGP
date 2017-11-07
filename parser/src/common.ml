open Operators
open Printf

type asn = Asn of int | Asn4 of int32

let asn_to_int = function
  | Asn a -> a
  | Asn4 a -> raise (Failure "Invalid asn: asn4 is not supported.")

let asn_to_string = function
  | Asn a -> sprintf "%d" a
  | Asn4 a ->
    if a < 65536_l then sprintf "%ld" a
    else
      sprintf "%ld.%ld" (a >>> 16) (a &&& 0xFFFF_l)

let asn_to_int = function
  | Asn a -> a
  | Asn4 a -> Int32.to_int a

let pfxlen_to_bytes l = (l+7) / 8

[%%cenum
type tc =
  | OPEN [@id 1]
  | UPDATE
  | NOTIFICATION
  | KEEPALIVE
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

type opt_param =
  | Reserved (* wtf? *)
  | Authentication (* deprecated, rfc 4271 *)
  | Capability of capability

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

type opent = {
  version: int;
  my_as: asn;
  hold_time: int;
  bgp_id: int32;
  options: opt_param list;
}



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

[%%cenum
type oc =
  | RESERVED [@id 0]
  | AUTHENTICATION
  | CAPABILITY
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

(* Attribute with extended length *)
[%%cstruct
type fte = {
  flags: uint8_t;
  tc: uint8_t;
  len: uint16_t
}
[@@big_endian]
]

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

type update = {
  withdrawn: Afi.prefix Cstruct.iter;
  path_attrs: path_attr Cstruct.iter;
  nlri: Afi.prefix Cstruct.iter;
}

type t =
  | Open of opent
  | Update of update
  | Notification
  | Keepalive


type path_attr_flag = {
  optional: bool;
  transitive: bool;
  partial: bool;
  extlen: bool;
}