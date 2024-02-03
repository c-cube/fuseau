(** Interoperability between Fuseau and Moonpool *)

val await_fut : 'a Moonpool.Fut.t -> 'a
(** [await_fut fut] suspends the current fiber until the given future is done *)
