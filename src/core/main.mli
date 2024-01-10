(** Main loop.

   This is the loop that runs both fibers, and the IO event loop,
   in an interspersed way.
*)

val main : ?max_tick_duration_us:int -> loop:Event_loop.t -> (unit -> 'a) -> 'a
(** [main f] runs [f()] in an event loop. The value is returned
    when the loop has nothing else to do, even if the particular
    computation was finished earlier.

    @param loop if provided, this event loop is used
    during the computation. *)
