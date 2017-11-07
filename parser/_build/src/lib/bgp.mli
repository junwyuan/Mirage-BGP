(*
 * Copyright (c) 2012-2015 Richard Mortier <mort@cantab.net>
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

[%%cstruct
  type h = {
    marker: uint8_t [@len 16];
    len: uint16_t;
    typ: uint8_t;
  }
  [@@big_endian]
]

type asn = Asn of int | Asn4 of int32
val asn_to_string: asn -> string

val pfxlen_to_bytes : int -> int
val get_nlri4 : Cstruct.t -> int -> Afi.prefix
val get_nlri6 : Cstruct.t -> int -> Afi.prefix

type caller = Normal | Table2 | Bgp4mp_as4
type path_attrs
val path_attrs_to_string : path_attrs -> string
val parse_path_attrs : ?caller:caller -> Cstruct.t -> path_attrs

type capability =
  | Mp_ext of Afi.tc * Safi.tc
  | Ecapability of Cstruct.t

type opt_param =
  | Reserved (* wtf? *)
  | Authentication (* deprecated, rfc 4271 *)
  | Capability of capability

type opent = {
  version: int;
  my_as: asn;
  hold_time: int;
  bgp_id: int32;
  options: opt_param list;
}
val opent_to_string : opent -> string

type update
val update_to_string : update -> string

type t =
  | Open of opent
  | Update of update
  | Notification
  | Keepalive
val to_string : t -> string
val parse : ?caller:caller -> Cstruct.t -> t Cstruct.iter
