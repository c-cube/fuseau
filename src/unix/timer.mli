open Common_

type t

val create : unit -> t
(** A new timer. *)

type tick_res =
  | Wait of float
  | Run of (event_handle -> unit) * event_handle
  | Empty

val next : t -> tick_res
val run_after : t -> float -> (event_handle -> unit) -> event_handle
val run_every : t -> float -> (event_handle -> unit) -> event_handle
val has_tasks : t -> bool
val num_tasks : t -> int
