(** Interoperability between Fuseau and Moonpool.

   The goal of this optional library is to make it
   easier to defer tasks to a {!Moonpool} thread pool
   from within [Fuseau]. Any CPU heavy task (e.g. cryptographic
   hashing, decoding large JSON values, sorting, etc.) that might
   take at least hundreds of microseconds might benefit from
   running in a thread pool; this also allows programs to
   use multiple cores event though the [Fuseau] event loop
   only runs on a single thread.

   For an example, see the web crawler [examples/lwt/argiope/argiope.ml]
   where the Moonpool runner is used to parse HTML pages
   in the background.
*)

val await_fut : 'a Moonpool.Fut.t -> 'a
(** [await_fut fut] suspends the current fiber until
    the given future is done. The future [fut] is a thread-safe
    Moonpool future (e.g. obtained from {!Moonpool.Fut.spawn})
    but this must be called from inside a fuseau thread. *)

val spawn : on:Moonpool.Runner.t -> (unit -> 'a) -> 'a Moonpool.Fut.t
(** An alias to {!Moonpool.Fut.spawn} *)

val spawn_and_await : on:Moonpool.Runner.t -> (unit -> 'a) -> 'a
