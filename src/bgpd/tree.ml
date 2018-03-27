(* Impure tree *)

type 'a t = {
  mutable parent: 'a t option;
  mutable children: 'a t list;
  mutable value: 'a;
}

let create parent children value = {
  parent; children; value;
}

let value t = t.value

let set_value t v = t.value <- v

let set_parent t p = t.parent = p

let add_child t c = t.children <- c::(t.children)

let add_child_v t v =
  let node = create (Some t) [] v in
  add_child t node;
  node
;; 

let rm_child_v t v = 
  let rec aux_rm = function 
    | [] -> []
    | hd::tl -> match hd.value = v with
      | true -> tl
      | false -> hd::(aux_rm tl)
  in
  t.children <- aux_rm t.children;
;;

let offspring t =
  let rec aux acc t =
    let f acc c = aux (c.value::acc) c in 
    List.fold_left f acc t.children 
  in
  aux [] t
;;

let rec root t =
  match t.parent with
  | None -> t
  | Some p -> root p
;;

let root_v t =
  let r = root t in
  r.value
;;

let ancestors t =
  let rec walkup acc t =
    match t.parent with
    | None -> acc
    | Some p -> walkup (p.value::acc) p
  in
  walkup [] t
;;

let () =
  let r = create None [] 1 in
  let n1 = add_child_v r 2 in
  let n2 = add_child_v r 3 in
  let n3 = add_child_v n2 4 in
  
  assert (offspring n3 = []);
  let l = offspring r in
  assert (List.length l = 3);
  assert (List.mem 2 l);
  assert (List.mem 3 l);
  assert (List.mem 4 l);

  assert (root_v n1 = 1);
  assert (root_v r = 1);

  assert (ancestors n2 = [1]);
  assert (ancestors n3 = [1; 3]);
;;



    