let s, push = Lwt_stream.create ();;

let t1 () = 
    push (Some "this is t1");
    Lwt.return_unit
;;

let t2 () =
    push (Some "this is t2");
    Lwt.return_unit;
;;

let _ = Lwt.pick[t1 (); t2 ()];;
