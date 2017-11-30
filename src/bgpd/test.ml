open Lwt.Infix
let f () = 
  let t, u = Lwt.task () in
  let t2 = 
    t >>= fun () ->
    Lwt_unix.sleep 3.
    >>= fun () ->
    Lwt_io.print "I am awke"
  in
  Lwt.wakeup u ();
  Lwt_unix.sleep 1. >>= fun () ->
  Lwt.cancel t2;
  t2
;;

let rec loop () =
  Lwt_io.print "Loop\n" 
  >>= fun () -> 
  Lwt_unix.sleep 3. 
  >>= fun () ->
  loop ()
;;

let rec loop2 () =
  Lwt_unix.sleep 3. 
  >>= fun () ->
  loop ()
;;

let rec timer () : unit Lwt.t = 
  let _ = Lwt_io.print "This is timer." in
  let t, u = Lwt.task () in
  let _ = Lwt_unix.sleep 3. >>= fun () -> Lwt.wakeup u (); Lwt.return_unit in
  let t2 = t >>= fun () -> timer () in
  let _ = loop () in
  t2
;;  

let () =
  let t = timer () in
  let t2 = Lwt_unix.sleep 7. >>= fun () -> Lwt.cancel t; Lwt.return_unit in
  Lwt_main.run (loop2 ())
;;



  
