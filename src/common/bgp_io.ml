module type S = sig
  type t
  type flow
  
  val create_connection : t -> (flow, string) Result.result Lwt.t
end