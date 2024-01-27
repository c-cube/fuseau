val await : 'a Lwt.t -> 'a
(** Like {!Fuseau.await} but on a Lwt promise. *)

val main : (unit -> 'a) -> 'a
(** Run main loop, using the current [Lwt_engine.t]. *)
