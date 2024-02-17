(** Interoperability between Fuseau and Moonpool *)

val await_fut : 'a Moonpool.Fut.t -> 'a
(** [await_fut fut] suspends the current fiber until
    the given future is done. The future [fut] is a thread-safe
    Moonpool future (e.g. obtained from {!Moonpool.Fut.spawn})
    but this must be called from inside a fuseau thread. *)

val spawn : on:Moonpool.Runner.t -> (unit -> 'a) -> 'a Moonpool.Fut.t
(** An alias to {!Moonpool.Fut.spawn} *)

val spawn_and_await : on:Moonpool.Runner.t -> (unit -> 'a) -> 'a
