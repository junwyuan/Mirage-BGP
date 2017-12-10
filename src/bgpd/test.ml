(* open Lwt

let () =
  let s = new Lwt_stream.bounded_push in


  let t = Lwt_stream.next s >>= fun _ -> Lwt_io.print "Got it." in
  (* let t2 = Lwt_unix.sleep 3. >>= fun () -> push None; Lwt.return_unit in *)
  let t3 = Lwt_stream.closed s in
  Lwt_main.run (Lwt.join [t; t3])
;; *)