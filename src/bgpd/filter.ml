(* Implementation of Policy Information Base and filtering *)
(* Implementation of Route-Map *)

type prefix_list = {
  list_name: string;
  entries: Ipaddr.V4.Prefix.t list;
}

let prefix_list_to_string l =
  let tmp = String.concat ";" (List.map (fun x -> Ipaddr.V4.Prefix.to_string x) l.entries) in
  Printf.sprintf "Prefix_list %s: [%s]" l.list_name tmp
;;

type condition = 
  | Prefix_list of prefix_list

let cond_to_string = function
  | Prefix_list l ->
    let tmp = String.concat ";" (List.map (fun x -> Ipaddr.V4.Prefix.to_string x) l.entries) in
    Printf.sprintf "Prefix_list %s: [%s]" l.list_name tmp
;;

type action =
  | Set_weight of int

let action_to_string = function
  | Set_weight w -> Printf.sprintf "Set weight %d" w
;;

type rme = {
  order: int;
  permit: bool;
  conditions: condition list;
  actions: action list;
}

let entry_to_string rme =
  let str_c = String.concat ";" (List.map cond_to_string rme.conditions) in
  let str_a = String.concat ";" (List.map action_to_string rme.actions) in
  Printf.sprintf "{ order: %d; permit: %b; conditions: %s; actions: %s }"
                  rme.order rme.permit str_c str_a
;;

type route_map = {
  map_name: string;
  entries: rme list;
}

let route_map_to_string route_map =
  let tmp = String.concat "\n" (List.map entry_to_string route_map.entries) in
  Printf.sprintf "route map %s\n%s" route_map.map_name tmp
;;

let create_route_map map_name = {
  map_name; entries = [];
}

let match_entry entry pfx attrs =
  let p = function
    | Prefix_list l -> List.mem pfx l.entries
  in
  List.for_all p entry.conditions
;;

let apply_entry entry _attrs = 
  let f (attrs, weight) = function
    | Set_weight w -> (_attrs, w)
  in
  List.fold_left f (_attrs, 0) entry.actions
;;

let add_entry route_map entry =
  let rec loop = function
    | [] -> [ entry ]
    | h::tl -> 
      if entry.order > h.order then h::(loop tl)
      else if entry.order = h.order then entry::tl
      else entry::(h::tl)
  in
  let tmp = loop route_map.entries in
  { map_name = route_map.map_name; entries = tmp }
;;

let apply route_map pfx _attrs =
  let rec loop (_attrs, weight) = function
    | [] -> None
    | h::tl -> match match_entry h pfx _attrs with
      | true -> 
        if h.permit then
          Some (apply_entry h _attrs)
        else None
      | false -> loop (_attrs, weight) tl
  in
  loop (_attrs, 0) route_map.entries
;;

let test () = 
  let pfx_list : prefix_list = {
    list_name = "1";
    entries = [
      Ipaddr.V4.Prefix.of_string_exn "10.10.0.0/16";
      Ipaddr.V4.Prefix.of_string_exn "10.11.0.0/16"
    ]
  } in

  let pfx_list2 : prefix_list = {
    list_name = "2";
    entries = [
      Ipaddr.V4.Prefix.of_string_exn "10.11.0.0/16"
    ]
  } in

  let entry : rme = {
    order = 20;
    permit = true;
    conditions = [
      Prefix_list pfx_list;
    ];
    actions = [
      Set_weight 20;
    ]
  } in

  let entry2 : rme = {
    order = 10;
    permit = true;
    conditions = [
      Prefix_list pfx_list2;
    ];
    actions = [
      Set_weight 50;
    ]
  } in

  let route_map : route_map = create_route_map "map1" in

  let route_map = add_entry route_map entry in
  let route_map = add_entry route_map entry2 in

  assert (apply route_map (Ipaddr.V4.Prefix.of_string_exn "10.10.0.0/16") [] = Some ([], 20));
  assert (apply route_map (Ipaddr.V4.Prefix.of_string_exn "10.11.0.0/16") [] = Some ([], 50));
  assert (apply route_map (Ipaddr.V4.Prefix.of_string_exn "10.12.0.0/16") [] = None);
;;






