open Lwt.Infix

module type S = sig
  type t
  val create : (unit -> unit Lwt.t) -> t
  val start : t -> unit Lwt.t
end

module Thread : S = struct
  type t = {
    mutable running: bool;
    task: unit -> unit Lwt.t
  }

  let rec run t = 
    if (t.running) then 
      t.task ()
      >>= fun () ->
      run t
    else Lwt.return_unit
  ;;

  let start t =
    t.running <- true;
    run t
  ;;

  let create task = {
    running = false;
    task;
  };;
end

let is_defined = function
  | None -> false
  | Some _ -> true
;;

