val sleep_ms : int -> unit
(** Suspend current fiber for the given number of milliseconds *)

val every : ?start_ms:int -> repeat_ms:int -> (unit -> unit) -> Disposable.t
(** Run function every [repeat_ms] milliseconds. *)
