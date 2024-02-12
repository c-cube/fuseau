(** Main loop *)

val main : loop:Event_loop.t -> (unit -> 'a) -> 'a
