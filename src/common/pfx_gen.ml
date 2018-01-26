type t = int32

let gen (seed: t) n =
  let pfxs = ref [] in
  let r = ref seed in
  for i = 1 to n do
    r := Int32.add !r 256_l; 
    let pfx = Ipaddr.V4.Prefix.make 24 (Ipaddr.V4.of_int32 !r) in
    pfxs := pfx::!pfxs;
  done;
  (!pfxs, !r)
;;

let peek_next_n seed n =
  Int32.add seed (Int32.mul 256_l (Int32.of_int n))
;;

let default_seed = Int32.shift_left 128_l 24