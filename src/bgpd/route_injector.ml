(* Only support UNIX for now *)

type route = {
  net: Ipaddr.V4.Prefix.t;
  gw: Ipaddr.V4.t option;
  iface: string option;
}

let route_to_string { net; gw; iface } =
  let s_iface = match iface with
    | None -> ""
    | Some v -> v
  in

  let s_gw = match gw with
    | None -> "*"
    | Some v -> Ipaddr.V4.to_string v
  in

  Printf.sprintf "%s %s %s" (Ipaddr.V4.Prefix.to_string net) s_gw s_iface
;;

module type S = sig
  type error
  val get_routes : unit -> (route list, error) result
  val add_route : route -> (unit, error) result
  val del_route : route -> (unit, error) result
end

module Mask = struct
  let masks =
    let rec gen n =
      if n >= 0 then
        (n, Ipaddr.V4.Prefix.mask n)::(gen (n - 1))
      else []
    in
    gen 32
  ;;

  let str_to_int str = 
    let tmp = Ipaddr.V4.of_string_exn str in
    let f (n, mask) = mask = tmp in
    match List.find_opt f masks with
    | None -> None
    | Some (bits, _) -> Some bits
  ;;
end

module Unix = struct
  type error = 
    | Exited of int
    | Signaled of int
    | Invalid_subnet
    | Invalid_mask
    | Invalid_gw
    | Unknown

  exception Err of error

  let get_routes () : (route list, error) result =  
    let cmd = Bos.Cmd.((v "route")) in
    let output = Bos.OS.Cmd.(run_out cmd |> out_lines) in
    match output with 
    | Result.Ok (output, (info, status)) -> begin
      match status with
      | `Exited code ->
        if code = 0 then 
          let f row =
            let aux = String.split_on_char (Char.chr 32) row in
            let cols = List.filter (fun x -> x <> "") aux in

            let ip =
              let tmp = List.nth cols 0 in
              if tmp = "default" then 
                Ipaddr.V4.unspecified
              else
                match Ipaddr.V4.of_string tmp with
                | None -> raise (Err Invalid_subnet)
                | Some ip -> ip
            in

            let bits = 
              let tmp = List.nth cols 2 in
              match Mask.str_to_int tmp with
              | None -> raise (Err Invalid_mask)
              | Some v -> v
            in

            let net = Ipaddr.V4.Prefix.make bits ip in

            let gw = 
              let tmp = List.nth cols 1 in
              if tmp = "*" then 
                None
              else
                match Ipaddr.V4.of_string tmp with
                | None -> raise (Err Invalid_gw)
                | Some v -> Some v
            in

            let iface = Some (List.nth cols 7) in

            { net; gw; iface }
          in
          
          try Result.Ok (List.map f (List.tl (List.tl output))) with
          | Err error -> Result.Error error
          | _ -> Result.Error Unknown
        else Result.Error (Exited code)
      | `Signaled code -> Result.Error (Signaled code)
    end
    | Result.Error _ -> Result.Error Unknown
  ;;


  let add_route { net; gw; iface } =
    let subnet = Ipaddr.V4.to_string (Ipaddr.V4.Prefix.network net) in
    
    let mask = Ipaddr.V4.to_string (Ipaddr.V4.Prefix.netmask net) in

    let gw = match gw with
      | None -> "*" 
      | Some v -> Ipaddr.V4.to_string v 
    in

    let cmd = Bos.Cmd.((v "route") % "add" % "-net" % subnet % "netmask" % mask % "gw" % gw) in
    let output = Bos.OS.Cmd.(run_out cmd |> out_lines) in
    match output with
    | Result.Ok (_, (_, status)) -> begin
      match status with
      | `Exited code -> 
        if code = 0 then
          Result.Ok ()
        else
          Result.Error (Exited code)
      | `Signaled code ->
        Result.Error (Signaled code)
    end
    | Result.Error _ -> Result.Error Unknown
  ;;

  let del_route { net; gw; iface } =
    let subnet = Ipaddr.V4.to_string (Ipaddr.V4.Prefix.network net) in
    
    let mask = Ipaddr.V4.to_string (Ipaddr.V4.Prefix.netmask net) in

    let gw = match gw with
      | None -> "*" 
      | Some v -> Ipaddr.V4.to_string v 
    in

    (* let cmd = Bos.Cmd.((v "route") % "del" % "-net" % subnet % "netmask" % mask % "gw" % gw) in *)
    let cmd = Bos.Cmd.((v "route") % "del" % "-net" % subnet % "netmask" % mask) in
    let output = Bos.OS.Cmd.(run_out cmd |> out_lines) in
    match output with
    | Result.Ok (_, (_, status)) -> begin
      match status with
      | `Exited code -> 
        if code = 0 then
          Result.Ok ()
        else
          Result.Error (Exited code)
      | `Signaled code ->
        Result.Error (Signaled code)
    end
    | Result.Error _ -> Result.Error Unknown
  ;;
end



(* let () =
  let bits = Mask.str_to_int "255.255.255.255" in
  assert (bits = Some 32);

  let net = Ipaddr.V4.(Prefix.make 16 (of_string_exn "10.11.0.0")) in
  let gw = Some (Ipaddr.V4.of_string_exn "172.19.0.253") in
  let iface = None in
  match Unix.del_route { net; gw; iface } with
  | Result.Ok () -> Printf.printf "Route added."
  | Result.Error _ -> Printf.printf "Error."

  match Unix.get_routes () with
  | Result.Ok routes ->
    List.iter (fun r -> Printf.printf "%s\n" (route_to_string r)) routes
  | Result.Error _ ->
    Printf.printf "Error!!!"
;; *)


