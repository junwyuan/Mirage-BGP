
(* This is a temporary workaround as modifying mrt-format directly is such a pain. *)
module Bgp_to_Rib: sig 
  val convert_update : Bgp.update -> Rib.update
end = struct
  let convert_ip ip = 
    match ip with
    | Afi.IPv4 ip -> Some (Ipaddr.V4.of_int32 ip)
    | Afi.IPv6 ip -> None
  ;;

  let convert_prefix (ip, mask) =
    match convert_ip ip with
    | Some addr -> Some (Ipaddr.V4.Prefix.make mask addr)
    | None -> None
  ;;

  let convert_pfx_list pfx_list =
    let opt_list = List.map (fun pfx -> convert_prefix pfx) pfx_list in
    List.fold_left (fun acc -> function None -> acc | Some v -> (List.cons v acc)) 
                   [] opt_list
  ;;

  let convert_update (u: Bgp.update) : Rib.update = 
    let open Bgp in
    let open Rib in
    {
      withdrawn = convert_pfx_list u.withdrawn;
      path_attrs = u.path_attrs;
      nlri = convert_pfx_list u.nlri
    }
  ;;
end

module Rib_to_Bgp : sig
  val convert_update : Rib.update -> Bgp.update
end = struct
  let convert_ip ip = 
    Afi.IPv4 (Ipaddr.V4.to_int32 ip)
  ;;

  let convert_prefix pfx : Afi.prefix = 
    let open Afi in
    let mask = Ipaddr.V4.Prefix.bits pfx in
    let addr = Ipaddr.V4.to_int32 (Ipaddr.V4.Prefix.network pfx) in
    (IPv4 addr, mask)
  ;;

  let convert_pfx_list pfx_list = 
    List.map (fun pfx -> convert_prefix pfx) pfx_list
  ;;

  let convert_update (u: Rib.update) : Bgp.update = 
    let open Bgp in
    let open Rib in
    {
      withdrawn = convert_pfx_list u.withdrawn;
      path_attrs = u.path_attrs;
      nlri = convert_pfx_list u.nlri;
    }
  ;;
end


