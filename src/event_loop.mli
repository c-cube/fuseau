(** Main event loop.

    Each value of this type is a full-fledged Libuv event
    loop, along with a scheduler for Picos fibers.
    This is the main entrypoint for this library.

    All computations happen in such an event loop. The loop
    is implicitly available in context thanks to thread-local-storage.

    There might be multiple event loops active at the same time,
    on different threads.
*)

type t
(** An event loop coupled with a scheduler. This is our main execution context. *)

val loop : t -> Luv.Loop.t
(** Luv loop associated with this *)

val spawn : t -> (unit -> 'a) -> ('a, [ `Await | `Cancel ]) Computation.t
(** Spawn a computation, from anywhere. *)

module Private : sig
  val get_current_exn : unit -> t
  (** Get local loop from within its execution. This will
      fail if it's not run from within an event loop. *)
end

val create_uv_loop : unit -> Luv.Loop.t

val main : ?loop:Luv.Loop.t -> (unit -> 'a) -> 'a
(** [main f] runs [f()] in an event loop. The value is returned
    when the loop has nothing else to do, even if the particular
    computation was finished earlier.

    @param loop if provided, this event loop is used
    during the computation. *)
